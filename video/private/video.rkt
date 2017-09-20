#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen, Benjamin Chung

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(provide (all-defined-out)
         mk-render-graph)
(require racket/dict
         racket/match
         racket/set
         racket/class
         racket/list
         racket/generic
         racket/hash
         racket/struct
         file/convertible
         (prefix-in file: file/convertible)
         graph
         "utils.rkt"
         "ffmpeg/main.rkt"
         "ffmpeg-pipeline.rkt"
         "init.rkt"
         "units.rkt"
         "devices.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(define current-render-graph (make-parameter (mk-render-graph)))
(define current-video-directory (make-parameter (current-directory)))
(define current-convert-database (make-parameter #f))

;; A helper function to convert videos to video nodes
;; Video (U Graph #f) -> _node
(define (convert source*
                 #:renderer [renderer* #f])
  (define source
    (cond [(video? source*) source*]
          [(video-convertible? source*)
           (video-convert source*)]
          [(and (current-convert-database)
                (send (current-convert-database) convertible? source*))
           (send (current-convert-database) convert)]
          [(file:convertible? source*) source*]
          [else (error 'convert "Object ~a cannot be converted to a video" source*)]))
  (parameterize ([current-render-graph (or renderer* (current-render-graph))])
    (struct default ())
    (define ret (file:convert source 'video (default)))
    (when (default? ret)
      (error 'convert "Object ~a cannot be compiled to video" source))
    ret))

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; avformat Path -> Void
(define (debug/save-prop avformat path)
  (av-dump-format avformat 0 path 0))

(define (finish-video-object-init video-node video-source)
  ;; Set user properties
  (define vprop
    (cond
      [(properties? video-source)
       (define prop (properties-prop video-source))
       (define demuxed-node
         (cond [(dict-ref prop "video-index" #f)
                video-node] ;; TODO
               [(dict-ref prop "audio-index" #f)
                video-node] ;; TODO
               [else video-node]))
       (define scaled-node
         (cond [(and (dict-ref prop "width" #f)
                     (dict-ref prop "height" #f))
                (define s-node
                  (mk-filter-node (hash 'video (mk-filter "scale"
                                                          (hash "width" (dict-ref prop "width")
                                                                "height" (dict-ref prop "height"))))
                                  #:props (dict-set* (or (node-props demuxed-node) (hash))
                                                     "width" (dict-ref prop "width")
                                                     "height" (dict-ref prop "height"))
                                  #:counts (node-counts video-node)))
                (add-vertex! (current-render-graph) s-node)
                (add-directed-edge! (current-render-graph) demuxed-node s-node 1)
                s-node]
               [else demuxed-node]))
       (define start
         (or (dict-ref prop "start" #f)
             (and (dict-ref prop "length" #f) 0)
             (and (dict-ref prop "end" #f) 0)))
       (define end
         (or (dict-ref prop "end" #f)
             (dict-ref prop "length" #f)))
       (define trimmed-prop
         (cond
           [(and start end (not (equal? end +inf.0)))
            (define node
              (mk-filter-node (hash 'video (mk-filter "trim" (hash "start" (racket->ffmpeg start)
                                                                   "end" (racket->ffmpeg end)))
                                    'audio (mk-filter "atrim" (hash "start" (racket->ffmpeg start)
                                                                    "end" (racket->ffmpeg end))))
                              #:props (dict-set* (node-props scaled-node)
                                                 "start" start
                                                 "end" end)
                              #:counts (node-counts scaled-node)))
            (add-vertex! (current-render-graph) node)
            (add-directed-edge! (current-render-graph) scaled-node node 1)
            node]
           [else scaled-node]))
       trimmed-prop]
      [else video-node]))
  ;; Attach filters
  (define attached
    (if (service? video-source)
        (for/fold ([ret vprop])
                  ([f (in-list (service-filters video-source))])
          (define f* (convert-filter f ret))
          (add-directed-edge! (current-render-graph) ret f* 1)
          f*)
        vprop))
  ;; Extend properties table (TODO)
  (define extended
    (cond [(properties? attached) attached]
          [else attached]))
  extended)

;; Because the file/convertible interface is too specialized, provide one
;;   that is specific to videos. This is re-exported (with contracts) in video/convert
(define-values (prop:video-convertible ~video-convertible? video-convertible-ref)
  (make-struct-type-property 'video-convertible))

;; Used to see if a particular _instance_ of a struct is convertible, even if
;;   the struct is convertible in general. If not provided it defaults to true.
(define-values (prop:video-convertible? ~video-convertible?? video-convertible?-ref)
  (make-struct-type-property 'video-convertible?))

;; Determines if a video is convertible
(define (video-convertible? v)
  (and (~video-convertible? v)
       (if (~video-convertible?? v)
           ((video-convertible?-ref v) v)
           #t)))

;; Convert to a video. (Assumes given a video-convertible?)
(define (video-convert v)
  ((video-convertible-ref v) v))

;; An interface for composing properties when rendering
;;   a multitrack or playlist
(define-generics composible
  (compose-playlist composible other)
  (compose-multitrack composible other))

;; Dynamic Dispatch for Video Objects
(define-generics video-ops
  (copy-video-op video-ops to-copy))
(define copy-video
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (unless (= 1 (length args))
       (error 'copy-video "copy-video requires exactly one non keyword argument"))
     (copy-video-op (first args)
                    (map cons
                         (map (compose string->symbol keyword->string) kws)
                         kw-args)))))

;; Constructor for video objects
(define current-detailed-printing? (make-parameter #f))
(define-syntax subclass-empty '(() () ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super* ([ids:id default] ...)
        ([convert-args:id convert-defaults] ...) body ...)
     #:with constructor (format-id stx "make-~a" #'name)
     #:with this (format-id stx "this")
     #:with new-supers (format-id stx "subclass-~a" #'name)
     #:with super (format-id stx "subclass-~a" (if (identifier? #'super*)
                                                   #'super*
                                                   #'empty))
     #:with predicate (format-id stx "~a?" #'name)
     #:with convert-name (format-id stx "convert-~a" #'name)
     (define super-vals (syntax-local-value #'super))
     (define all-structs (append (first super-vals) (make-list (length (syntax->list #'(ids ...)))
                                                               (syntax->datum #'name))))
     (define all-ids (append (second super-vals) (syntax->datum #'(ids ...))))
     (define all-defaults (append (third super-vals) (syntax-e #'(default ...))))
     (quasisyntax/loc stx
       (begin
         (struct name #,@(if (identifier? #'super*) (list #'super*) '())
           (ids ...)
           #:methods gen:dict
           [(define dict-ref get-property)
            (define dict-set set-property)
            (define dict-remove remove-property)]
           #:methods gen:custom-write
           [(define (write-proc vid port mode)
              (if (current-detailed-printing?)
                  ((make-constructor-style-printer
                    (λ (obj) '#,#'name)
                    (λ (obj)
                      (list #,@(for/list ([i (in-list all-structs)]
                                          [j (in-list all-ids)])
                                 #`(#,(format-id stx "~a-~a" i j) obj)))))
                   vid port mode)
                  (fprintf port "#<~a>" '#,#'name)))]
           #:methods gen:video-ops
           [(define (copy-video-op v to-copy)
              (name
               #,@(for/list ([i (in-list all-structs)]
                             [j (in-list all-ids)])
                    #`(if (dict-has-key? to-copy '#,j)
                          (dict-ref to-copy '#,j)
                          (#,(format-id stx "~a-~a" i j) v)))))]
           #:property prop:convertible
           (let ([memo-table (make-hasheq)])
             (λ (v request def)
               (match request
                 ['video
                  (if #t ;(current-skip-memoize?)
                      (convert-name v)
                      (hash-ref! memo-table v convert-name))]
                 [_ def]))))
         #,(quasisyntax/loc stx
             (define (convert-name v [convert-args convert-defaults] ...)
               (define ret
                 (call-with-values
                  (λ ()
                    (let ([this v])
                      (let #,(for/list ([i (in-list all-structs)]
                                        [j (in-list all-ids)])
                               #`[#,(datum->syntax stx j)
                                  (#,(format-id stx "~a-~a" i j) v)])
                        #f body ...)))
                  list))
               (define finished
                 (for/list ([i (in-list ret)])
                   (and i
                        (finish-video-object-init i v))))
               (apply values finished)))
         #,(quasisyntax/loc stx
             (define (constructor #,@(append*
                                      (for/list ([i (in-list all-ids)]
                                                 [j (in-list all-defaults)])
                                        `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                          [,(datum->syntax stx i) ,j]))))
               (name #,@(map (curry datum->syntax stx) all-ids))))
         (define-syntax new-supers '#,(list all-structs all-ids all-defaults))))]))

;; Properties functions
;; These need to come above the structs so they can take advantage of the
;;   gen:prop interface.

(struct not-found-key ())
(define (get-property dict key
                      [default (λ () (error 'get-property "Key not found: ~a" key))])
  (define the-dict 
    (cond [(properties? dict) (properties-prop dict)]
          [(node? dict) (node-props dict)]
          [else (error 'get-property "Not a valid video object: ~a" dict)]))
  (cond
    [(equal? key "length")
     (define maybe-length (dict-ref the-dict key #f))
     (or maybe-length
         (and (dict-ref the-dict "start" #f)
              (dict-ref the-dict "end" #f)
              (- (dict-ref the-dict "end")
                 (dict-ref the-dict "start")))
         (if (video? dict)
             (parameterize ([current-render-graph (mk-render-graph)])
               (define new-dict (node-props (convert dict)))
               (and (dict-ref new-dict "start" #f)
                    (dict-ref new-dict "end" #f)
                    (- (dict-ref new-dict "end" default)
                       (dict-ref new-dict "start" default))))
             (dict-ref the-dict key default)))]
    [(video? dict)
     (define maybe-val (dict-ref the-dict key (not-found-key)))
     (if (not-found-key? maybe-val)
         (parameterize ([current-render-graph (mk-render-graph)])
           (define new-dict (node-props (convert dict)))
           (dict-ref new-dict key default))
         maybe-val)]
    [else (dict-ref the-dict key default)]))
(define (set-property obj key val)
  (define new-props (hash-set (properties-prop obj) key val))
  (copy-video obj #:prop new-props))
(define (remove-property obj key)
  (define new-props (hash-remove (properties-prop obj) key))
  (copy-video obj #:prop new-props))

;; Structs
(define-constructor video #f () ())

(define-constructor chapter video ([start #f]
                                   [end #f])
  ())

;; Helper function to updated a list of chapters to be offset by their clip.
(define (update-chapters-list chapters offset)
  (for/list ([c (in-list chapters)])
    (copy-video c
                #:start (+ offset (chapter-start c))
                #:end (+ offset (chapter-end c)))))

(define-constructor properties video ([prop (hash)]) ())

(define-constructor service properties ([filters '()]) ())

(define-constructor filter service ([subgraph #f])
  ([prev #f])
  (unless prev
    (error 'filter "Prev node ~a not found" prev))
  (cond
    [(dict? subgraph)
     (define node (mk-filter-node subgraph
                                  #:counts (if prev (node-counts prev) (hash))
                                  #:props (if prev (node-props prev) (hash))))
     (add-vertex! (current-render-graph) node)
     node]
    [(procedure? subgraph)
     (define rg (subgraph (weighted-graph/directed '()) prev))
     (graph-union! (current-render-graph) (video-subgraph-graph rg))
     (add-directed-edge! (current-render-graph) prev (video-subgraph-sources rg) 1)
     (video-subgraph-sinks rg)]))

(define-constructor nullsink service ([source #f])
  ()
  (define node (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                                     'audio (mk-empty-sink-audio-filter))))
  (add-vertex! (current-render-graph) node)
  (when source
    (define s-node (convert source))
    (add-vertex! (current-render-graph) s-node)
    (add-directed-edge! s-node node 1))
  node)

(define-constructor transition service ([track1-subgraph #f]
                                        [track2-subgraph #f]
                                        [combined-subgraph #f])
  ([prev1 #f]
   [prev2 #f])
  (unless (and prev1 prev2)
    (error 'transition "Expected prev nodes, found ~a and ~a" prev1 prev2))
  (define prev1-copy (mk-split-node #:fan-out 2
                                    #:counts (node-counts prev1)
                                    #:props (node-props prev1)))
  (define prev2-copy (mk-split-node #:fan-out 2
                                    #:counts (node-counts prev2)
                                    #:props (node-props prev2)))
  (add-vertex! (current-render-graph) prev1-copy)
  (add-vertex! (current-render-graph) prev2-copy)
  (add-directed-edge! (current-render-graph) prev1 prev1-copy 1)
  (add-directed-edge! (current-render-graph) prev2 prev2-copy 1)
  (define track1-sub (track1-subgraph (weighted-graph/directed '()) prev1))
  (define track2-sub (track2-subgraph (weighted-graph/directed '()) prev2))
  (define combined-sub (combined-subgraph (weighted-graph/directed '()) prev1 prev2))
  (define r1
    (cond [track1-sub
           (graph-union! (current-render-graph) (video-subgraph-graph track1-sub))
           (add-directed-edge! (current-render-graph)
                               prev1-copy
                               (video-subgraph-sources track1-sub)
                               2)
           (video-subgraph-sinks track1-sub)]
          [else
           (define sink-node (mk-empty-sink-node #:counts (node-counts prev1-copy)))
           (add-vertex! (current-render-graph) sink-node)
           (add-directed-edge! (current-render-graph) prev1-copy sink-node 2)
           #f]))
  (define r2
    (cond [track2-sub
           (graph-union! (current-render-graph) (video-subgraph-graph track2-sub))
           (add-directed-edge! (current-render-graph)
                               prev2-copy (video-subgraph-sources track2-sub) 1)
           (video-subgraph-sinks track2-sub)]
          [else
           (define sink-node (mk-empty-sink-node #:counts (node-counts prev2-copy)))
           (add-vertex! (current-render-graph) sink-node)
           (add-directed-edge! (current-render-graph) prev2-copy sink-node 1)
           #f]))
  (define rc
    (cond [combined-sub
           (graph-union! (current-render-graph) (video-subgraph-graph combined-sub))
           (add-directed-edge! (current-render-graph)
                               prev1-copy (car (video-subgraph-sources combined-sub)) 1)
           (add-directed-edge! (current-render-graph)
                               prev2-copy (cdr (video-subgraph-sources combined-sub)) 2)
           (video-subgraph-sinks combined-sub)]
          [else
           (define sink-node1 (mk-empty-sink-node #:counts (node-counts prev1-copy)))
           (define sink-node2 (mk-empty-sink-node #:counts (node-counts prev2-copy)))
           (add-vertex! (current-render-graph) sink-node1)
           (add-vertex! (current-render-graph) sink-node2)
           (add-directed-edge! (current-render-graph) prev1-copy sink-node1 1)
           (add-directed-edge! (current-render-graph) prev2-copy sink-node2 2)
           #f]))
  (values r1 r2 rc))

(define-constructor producer service ([subgraph (hash)])
  ([prev #f])
  (cond
    [(dict? subgraph)
     (define node (mk-filter-node subgraph
                                  #:counts (for/hash ([(k v) (in-dict subgraph)])
                                             (values k 1))
                                  #:props prop))
     (add-vertex! (current-render-graph) node)
     (when prev
       (add-directed-edge! (current-render-graph) prev node 1))
     node]
    [(procedure? subgraph)
     (unless prev
       (error 'producer "Undefined prev node ~a" prev))
     (define rg (subgraph prev))
     (graph-union! (current-render-graph) (video-subgraph-graph rg))
     (add-directed-edge! (current-render-graph) prev (video-subgraph-sources rg))
     (video-subgraph-sinks rg)]))

(define-constructor file producer ([path #f]) ()
  (when (not path)
    (error 'file "No path given"))
  (define bundle (file->stream-bundle path))
  (define fstart #f)
  (define fduration #f)
  (define fctx (stream-bundle-avformat-context bundle))
  (define fwidth #f)
  (define fheight #f)
  (define ftime-base #f)
  (define ffps #f)
  (define fpix-fmt #f)
  (define fsample-fmt #f)
  (define fsample-rate #f)
  (define fchapters (dict-ref prop "chapters" '()))
  (define count-tab (make-hash))
  (define error-when-mismatch? #f) ;; For debugging
  (define-syntax-rule (set!/error var val comp)
    (cond [(and var error-when-mismatch? (comp var val))
           (error 'file "Incompatible stream values ~a and ~a" var val)]
          #|
          [(pair? var)
           (set! var (cons val var))]
          [var
           (set! var (list val var))]
|#
          [else (set! var val)]))
  (for ([str (stream-bundle-streams bundle)])
    (match str
      [(struct* codec-obj ([codec-context cctx]
                           [type type]
                           [stream stream]))
       (hash-update! count-tab type
                     add1
                     0)
       (match type
         ['video
          (set!/error fwidth (avcodec-context-width cctx) =)
          (set!/error fheight (avcodec-context-height cctx) =)
          (set!/error ffps (avcodec-context-framerate cctx) =)
          (set!/error ftime-base (avstream-time-base stream) =)
          (define avst (avstream-start-time stream))
          (set!/error fstart
                      (if (= avst AV-NOPTS-VALUE) #f avst)
                      =)
          (set!/error fduration (avstream-duration stream) =)
          (set!/error fpix-fmt (avcodec-context-pix-fmt cctx) eq?)]
         ['audio
          (set!/error fsample-fmt (avcodec-context-sample-fmt cctx) eq?)
          (set!/error fsample-rate (avcodec-context-sample-rate cctx) =)]
         [_ (void)])]))
  (define props
    (hash "start" (or (and fstart ftime-base
                           (* fstart ftime-base))
                      0)
          "end" (or (and fstart fduration ftime-base
                         (* (+ fduration fstart) ftime-base))
                    +inf.0)
          "width" (or fwidth 0)
          "height" (or fheight 0)
          "time-base" (or ftime-base 0)
          "fps" (or ffps 0)
          "pix-fmt" fpix-fmt
          "sample-fmt" fsample-fmt
          "sample-rate" fsample-rate
          "chapters" fchapters))
  (define node (mk-source-node bundle
                               #:counts count-tab
                               #:props props))
  (add-vertex! (current-render-graph) node)
  (cond [(and fstart fduration ftime-base)
         node]
        [else (define b-node (convert (make-blank #:prop (hash "width" fwidth
                                                               "height" fheight))))
              (define c-node
                (mk-filter-node (hash 'video (mk-filter "overlay" (hash "x" 0
                                                                        "y" 0)))
                                #:props props
                                #:counts count-tab))
              (add-vertex! (current-render-graph) c-node)
              (add-directed-edge! (current-render-graph) b-node c-node 1)
              (add-directed-edge! (current-render-graph) node c-node 2)
              c-node]))

(define-constructor input-device producer ([video #f]
                                           [audio #f])
  ()
  (define devices (list-input-devices))
  (define vid-str (index-of (input-devices-video devices) video))
  (define aud-str (index-of (input-devices-audio devices) audio))
  (define bundle (devices->stream-bundle vid-str aud-str))
  (define node (mk-source-node bundle
                               #:counts (+ (if vid-str 1 0)
                                           (if aud-str 1 0))))
  (add-vertex! (current-render-graph) node)
  node)

(define-constructor blank producer () ()
  (define start (dict-ref prop "start" #f))
  (define end (dict-ref prop "end" #f))
  (mk-filter-node (hash 'video (mk-empty-video-filter #:width (dict-ref prop "width" #f)
                                                      #:height (dict-ref prop "height" #f)
                                                      #:duration (and end start (- end start)))
                        'audio (mk-empty-audio-filter))
                  #:counts (hash 'video 1 'audio 1)
                  #:props prop))

(define-constructor playlist producer ([elements '()])
  ()
  (define pre-list
    (cond
      [(empty? elements) (list (make-blank #:prop (hash "start" 0
                                                        "end" 1)))]
      [(transition? (first elements))
       (list (make-blank #:prop (hash "start" 0
                                      "end" (get-property (first elements) "pre-length"))))]
      [else (list)]))
 (define post-list
    (cond
      [(empty? elements) (list)]
      [(transition? (last elements))
       (list (make-blank #:prop (hash "start" 0
                                      "end" (get-property (last elements) "post-length"))))]
      [else (list)]))
  (define elements*
    (append pre-list elements post-list))
  ;; Generate nodes and handle transitions
  ;; When a transition is encountered, split previous node, pass through
  ;;    track1 filter.
  ;; Then store new filter as well as the creators for track2 and
  ;;    combined subgraph.
  ;; On the next video, split and pass to track2 and combined subgraph.
  ;; Otherwise just add the compiled filter to the list.
  ;; Invariant assumed: Transitions are never the first or last filters in a list.
  ;; Invariant assumed: No two transitions happen side by side.
  (define-values (rev-trans-vids rev-trans-nodes start end fps width height counts chapters)
    (for/fold ([prev-vids '()]
               [prev-nodes '()]
               [start 0]
               [end 0]
               [fps 0]
               [width 0]
               [height 0]
               [counts (hash)]
               [chapters '()])
              ([i (in-list elements*)]
               [index (in-naturals)])
      (match i
        [(struct* transition ([track1-subgraph track1-subgraph-proc]
                              [track2-subgraph track2-subgraph-proc]
                              [combined-subgraph combined-subgraph-proc]))
         (define track1-copy (mk-filter-node (hash 'video (mk-filter "split")
                                                   'audio (mk-filter "asplit"))
                                             #:props (node-props (car prev-nodes))
                                             #:counts (node-counts (car prev-nodes))))
         (add-directed-edge! (current-render-graph) (car prev-nodes) track1-copy 1)
         (define track1-subgraph (track1-subgraph-proc (mk-render-graph) (car prev-nodes)))
         (cond [(video-subgraph? track1-subgraph)
                (define length-track1 (get-property track1-subgraph "length" 0))
                (define source (video-subgraph-sources track1-subgraph))
                (define sink (video-subgraph-sinks track1-subgraph))
                (graph-union! (current-render-graph) (video-subgraph-graph track1-subgraph))
                (add-directed-edge! (current-render-graph) track1-copy source 2)
                (define clip-offset (- length-track1))
                (values (cons i prev-vids)
                        (list* track1-copy sink (cdr prev-nodes))
                        start
                        (+ end clip-offset)
                        fps
                        width
                        height
                        (hash-union counts (node-counts (car prev-nodes) #:combine max))
                        (update-chapters-list chapters clip-offset))]
               [else
                (define sink
                  (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                                        'audio (mk-empty-sink-audio-filter))
                                  #:counts (node-counts (car prev-nodes))))
                (add-vertex! (current-render-graph) sink)
                (add-directed-edge! (current-render-graph) track1-copy sink 2)
                (define clip-offset ;; Double `-` for clarity
                  (- (- (get-property (car prev-nodes) "end")
                        (get-property (car prev-nodes) "start"))))
                (values (cons i prev-vids)
                        (cons track1-copy (cdr prev-nodes))
                        start
                        (+ end clip-offset)
                        fps
                        width
                        height
                        counts
                        (update-chapters-list chapters clip-offset))])]
        [_
         (define pre-node (convert i))
         (define node
           (cond [(and (not (null? prev-vids))
                       (transition? (car prev-vids)))
                  (define track2-copy (mk-filter-node (hash 'video (mk-filter "split")
                                                            'audio (mk-filter "asplit"))
                                                      #:props (node-props pre-node)
                                                      #:counts (node-counts pre-node)))
                  (add-vertex! (current-render-graph) track2-copy)
                  (add-directed-edge! (current-render-graph) pre-node track2-copy 1)
                  (define track1-copy (car prev-nodes))
                  (define track2-subgraph-proc (transition-track2-subgraph (car prev-vids)))
                  (define combined-subgraph-proc (transition-combined-subgraph (car prev-vids)))
                  (define track2-subgraph (track2-subgraph-proc (mk-render-graph) pre-node))
                  (define combined-subgraph
                    (combined-subgraph-proc (mk-render-graph) track1-copy track2-copy))
                  (cond [(video-subgraph? track2-subgraph)
                         (graph-union! (current-render-graph) (video-subgraph-graph track2-subgraph))
                         (add-directed-edge! (current-render-graph)
                                             track2-copy
                                             (video-subgraph-sources track2-subgraph)
                                             1)]
                        [else
                         (define sink
                           (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                                                 'audio (mk-empty-sink-audio-filter))
                                           #:counts (node-counts pre-node)))
                         (add-vertex! (current-render-graph) sink)
                         (add-directed-edge! (current-render-graph) track2-copy sink 1)])
                  (cond [(video-subgraph? combined-subgraph)
                         (graph-union! (current-render-graph)
                                       (video-subgraph-graph combined-subgraph))
                         (add-directed-edge! (current-render-graph)
                                             track2-copy
                                             (cdr (video-subgraph-sources combined-subgraph))
                                             2)
                         (add-directed-edge! (current-render-graph)
                                             track1-copy
                                             (car (video-subgraph-sources combined-subgraph))
                                             1)]
                        [else
                         (define sink-a
                           (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                                                 'audio (mk-empty-sink-audio-filter))
                                           #:counts (node-counts pre-node)))
                         (add-vertex! (current-render-graph) sink-a)
                         (add-directed-edge! (current-render-graph)
                                             track1-copy
                                             sink-a
                                             1)
                         (define sink-b
                           (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                                                 'audio (mk-empty-sink-audio-filter))))
                         (add-vertex! (current-render-graph) sink-b)
                         (add-directed-edge! (current-render-graph)
                                             track2-copy
                                             sink-b
                                             2)])
                  (hash 'combined (and combined-subgraph (video-subgraph-sinks combined-subgraph))
                        'combined-subgraph combined-subgraph
                        'track2 (and track2-subgraph (video-subgraph-sinks track2-subgraph))
                        'track2-subgraph track2-subgraph)]
                 [else pre-node]))
         (cond [(dict? node)
                (define clip-offset
                  (- (if (dict-ref node 'combined-subgraph)
                      (get-property (dict-ref node 'combined-subgraph) "length")
                      0)
                     (if (dict-ref node 'track2-subgraph)
                         (get-property (dict-ref node 'track2-subgraph) "length")
                         0)))
                (values (cons i prev-vids)
                        (append
                         (append
                          (if (dict-ref node 'track2)
                                            (list (dict-ref node 'track2))
                                            '())
                          (if (dict-ref node 'combined)
                              (list (dict-ref node 'combined))
                              '()))
                         (cdr prev-nodes))
                        start
                        (+ end clip-offset)
                        fps
                        width
                        height
                        counts
                        (append (get-property pre-node "chapters" '())
                                (update-chapters-list chapters clip-offset)))]
               [else
                (define props (node-props node))
                (define clip-offset (- (dict-ref props "end") (dict-ref props "start")))
                (values (cons i prev-vids)
                        (cons node prev-nodes)
                        start
                        (+ end clip-offset)
                        (max fps (dict-ref props "fps" 0))
                        (max width (dict-ref props "width" 0))
                        (max height (dict-ref props "height" 0))
                        (hash-union counts (node-counts pre-node) #:combine max)
                        (append (get-property pre-node "chapters" '())
                                (update-chapters-list chapters clip-offset)))])])))
  (define transition-nodes (reverse rev-trans-nodes))
  (define transition-videos (reverse rev-trans-vids))
  ;; Remove now unneded transitions
  ;; Go through again and fix frame rates and aspect ratios
  (define cleaned-nodes
    (for/list ([node (in-list transition-nodes)])
      (define (coerce-clip vid-filter connect-node)
        (define node
          (mk-filter-node
           (hash 'video vid-filter)
           #:props (dict-copy (node-props connect-node))
           #:counts counts))
        (add-vertex! (current-render-graph) node)
        (add-directed-edge! (current-render-graph) connect-node node 1)
        node)
      (let* ([ret (if (and (> width 0)
                           (> height 0))
                      (coerce-clip (mk-filter "scale" (hash "width" width
                                                            "height" height))
                                   node)
                      node)]
             [ret (if (> fps 0)
                      (coerce-clip (mk-filter "fps" (hash "fps" fps))
                                   ret)
                      ret)]
             [ret (coerce-clip (mk-filter "format" (hash "pix_fmts" "yuv420p"))
                               ret)])
        ret)))
  ;; Offset each clip accordingly
  (define-values (prev-nodes time-again)
    (for/fold ([prev-nodes '()]
               [time 0])
              ([n (in-list cleaned-nodes)])
      (define props (node-props n))
      (define start (dict-ref props "start"))
      (define end (dict-ref props "end"))
      (define offset (mk-filter "setpts" (hash "expr" "PTS-STARTPTS")))
      (define aoffset (mk-filter "asetpts" (hash "expr" "PTS-STARTPTS")))
      (define props* (dict-copy props))
      (dict-set*! props*
                  "start" 0
                  "end" (+ end time))

      (define node
        (mk-filter-node (hash 'video offset
                              'audio aoffset)
                        #:props props*
                        #:counts counts))
      (add-vertex! (current-render-graph) node)
      (add-directed-edge! (current-render-graph) n node 1)
      (values (cons node prev-nodes) (+ time (- end start)))))
  (define nodes (reverse prev-nodes))
  ;; Build backing structure and transition everything to it.
  (define the-playlist
    (mk-filter-node (hash 'video (mk-filter "concat"
                                            (hash "n" (length nodes)
                                                  "v" 1
                                                  "a" 0))
                          'audio (mk-filter "concat"
                                            (hash "n" (length nodes)
                                                  "v" 0
                                                  "a" 1)))
                    #:props (hash "start" start
                                  "end" end
                                  "fps" fps
                                  "width" width
                                  "height" height)
                    #:counts counts))
  (add-vertex! (current-render-graph) the-playlist)
  (for ([i (in-list nodes)]
        [index (in-naturals)])
    (define pre-buffer (mk-filter-node (hash 'video (mk-filter "fifo")
                                             'audio (mk-filter "afifo"))
                                       #:counts counts))
    (add-vertex! (current-render-graph) pre-buffer)
    (add-directed-edge! (current-render-graph) i pre-buffer 1)
    (add-directed-edge! (current-render-graph) pre-buffer the-playlist index))
  (define the-buffer
    (mk-filter-node (hash 'video (mk-filter "fifo") 'audio (mk-filter "afifo"))
                    #:props (hash "start" start
                                  "end" end
                                  "fps" fps
                                  "width" width
                                  "height" height
                                  "chapters" chapters)
                    #:counts counts))
  (add-vertex! (current-render-graph) the-buffer)
  (add-directed-edge! (current-render-graph) the-playlist the-buffer 1)
  the-buffer)

(define-constructor multitrack producer ([tracks '()] [field '()])
  ()
  ;; Make the tracks
  (define-values (raw-nodes start end)
    (for/fold ([raw-nodes (hash)]
               [start 0]
               [end 0])
              ([t (in-list tracks)]
               [index (in-naturals)])
      (define node (convert t))
      (define new-end (get-property node "end" +inf.0))
      (values (dict-set raw-nodes t (cons node index))
              (min start (get-property node "start" 0))
              (if (= new-end +inf.0)
                  end
                  (max end new-end)))))
  ;; Convert all clips to a compatible length
  ;; Nodes : Hash[Track -> (Cons Node Integer)]
  ;; Where the key is the source node, and the value is
  ;;   its trimmed counterpart.
  (define nodes
    (hash-copy
     (for/hash ([(k v) (in-dict raw-nodes)])
       (define trimmed (mk-trim-node #:start start
                                     #:end end
                                     #:counts (node-counts (car v))
                                     #:props (node-props (car v))))
       (add-vertex! (current-render-graph) trimmed)
       (add-directed-edge! (current-render-graph) (car v) trimmed 1)
       (values k (cons trimmed (cdr v))))))
  ;; Make a back map, this is so we can have merging semantics
  ;;   when transition are applied
  (define back-map
    (hash-copy
     (for/hash ([(k v) (in-dict nodes)])
       (values v (set k)))))
  ;; Start merging tracks together
  ;; Prefer individual track if it exists, otherwise
  ;;   merge combined one.
  (for ([f (in-list field)])
    (match f
      [(struct* field-element ([element element]
                               [track track]
                               [track-2 track-2]))
       ;((dynamic-require 'racket/pretty 'pretty-print) nodes)
       ;(newline)
       ;((dynamic-require 'racket/pretty 'pretty-print) back-map)
       ;(newline)
       ;(displayln "-----")
       ;(newline)
       (define bundle-pair (dict-ref nodes track))
       (define bundle-pair-back (dict-ref back-map bundle-pair))
       (for ([tr (in-set bundle-pair-back)])
         (dict-remove! nodes tr))
       (dict-remove! back-map bundle-pair)
       (define bundle-pair-2 (dict-ref nodes track-2))
       (define bundle-pair-2-back (dict-ref back-map bundle-pair-2))
       (for ([tr (in-set bundle-pair-2-back)])
         (dict-remove! nodes tr))
       (dict-remove! back-map bundle-pair-2)
       (define-values (track1-out track2-out combined-out)
         (convert-transition element (car bundle-pair) (car bundle-pair-2)))
       (when combined-out
         (define idx (min (cdr bundle-pair) (cdr bundle-pair-2)))
         (define back-set (set-union (if track1-out (set) bundle-pair-back)
                                     (if track2-out (set) bundle-pair-2-back)))
         (define val (cons combined-out idx))
         (dict-set! back-map val back-set)
         (for ([tr (in-set back-set)])
           (dict-set! nodes tr val)))
       (when track1-out
         (define val (cons track1-out (cdr bundle-pair)))
         (dict-set! back-map val bundle-pair-back)
         (for ([tr (in-set bundle-pair-back)])
           (dict-set! nodes tr val)))
       (when track2-out
         (define val (cons track2-out (cdr bundle-pair-2)))
         (dict-set! back-map val bundle-pair-2-back)
         (for ([tr (in-set bundle-pair-2-back)])
           (dict-set! nodes tr)))]))
  ;; Select top-most track from table
  (define-values (ret trash)
    (for/fold ([node #f]
               [level -1])
              ([(track n-pair) (in-dict nodes)])
      (if (<= level (cdr n-pair))
          (values (car n-pair) (cdr n-pair))
          (values node level))))
  ;; Return top-most track, throw away rest
  (for ([(track n-pair) (in-dict nodes)])
    (unless (equal? (car n-pair) ret)
      (define trash (convert (make-nullsink)))
      (add-vertex! (current-render-graph) trash)
      (add-directed-edge! (current-render-graph) (car n-pair) trash 1)))
  ret)

(define-constructor video-subgraph properties ([graph (mk-render-graph)]
                                               [sinks '()]
                                               [sources '()])
  ()
  (graph-union! (current-render-graph) graph)
  (car sources))

(define-constructor field-element video ([element #f] [track #f] [track-2 #f]) ())
