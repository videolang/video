#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

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

(provide (all-defined-out))
(require racket/dict
         racket/match
         racket/set
         racket/class
         racket/list
         racket/generic
         racket/hash
         file/convertible
         (prefix-in file: file/convertible)
         graph
         "utils.rkt"
         "ffmpeg.rkt"
         "ffmpeg-pipeline.rkt"
         "init.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(define current-render-graph (make-parameter (mk-render-graph)))
(define (mk-render-graph) (weighted-graph/directed '()))
(define current-video-directory (make-parameter (current-directory)))

;; A helper function to convert videos to video nodes
;; Video (U Graph #f) -> _node
(define (convert source
                 #:renderer [renderer* #f])
  (parameterize ([current-render-graph (or renderer* (current-render-graph))])
    (file:convert source 'video)))

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; Properties Path -> Void
(define (debug/save-prop prop filepath)
  (error "TODO"))

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
         (mk-filter-node (hash 'video (mk-filter "pad"
                                                 (hash "width" (dict-ref prop "width" "iw")
                                                       "height" (dict-ref prop "height" "ih")
                                                       "x" (dict-ref prop "x" "0")
                                                       "y" (dict-ref prop "y" "0"))))
                         #:props (node-props video-node)
                         #:counts (node-counts video-node)))
       (add-vertex! (current-render-graph) scaled-node)
       (add-directed-edge! (current-render-graph) demuxed-node scaled-node 1)
       (define start
         (or (dict-ref prop "start" #f)
             (and (dict-ref prop "length" #f) 0)
             (and (dict-ref prop "end" #f) 0)))
       (define end
         (or (dict-ref prop "end" #f)
             (dict-ref prop "length" #f)))
       (define trimmed-prop
         (cond
           [(and start end)
            (define node
              (mk-filter-node (hash 'video (mk-filter "trim" (hash "start" start
                                                                   "end" end))
                                    'audio (mk-filter "atrim" (hash "start" start
                                                                    "end" end)))
                              #:props (node-props scaled-node)
                              #:counts (node-counts scaled-node)))
            (add-vertex! (current-render-graph) node)
            (add-directed-edge! (current-render-graph) scaled-node node)
            node]
           [else scaled-node]))
       trimmed-prop]
      [else video-node]))
  ;; Attach filters
  (if (service? video-source)
      (for/fold ([ret vprop])
                ([f (in-list (service-filters video-source))])
        (define f* (convert f))
        (add-directed-edge! (current-render-graph) ret f* 1)
        f*)
      vprop))

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
(define-syntax subclass-empty '(() () ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super* ([ids:id default] ...)
        ([convert-args:id convert-default] ...) body ...)
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
           #:transparent
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
         (define (convert-name v [convert-args convert-defaults] ...)
           (define ret
             (let ([this v])
               (let #,(for/list ([i (in-list all-structs)]
                                 [j (in-list all-ids)])
                        #`[#,(datum->syntax stx j)
                           (#,(format-id stx "~a-~a" i j) v)])
                 #f body ...)))
           (finish-video-object-init ret v))
         (define (constructor #,@(append*
                                  (for/list ([i (in-list all-ids)]
                                             [j (in-list all-defaults)])
                                    `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                      [,(datum->syntax stx i) ,j]))))
           (name #,@(map (curry datum->syntax stx) all-ids)))
         (define-syntax new-supers '#,(list all-structs all-ids all-defaults))))]))

;; Structs
(define-constructor video #f () ())

(define-constructor link video ([source #f] [target #f] [index 0])
  ()
  (error "TODO"))

(define-constructor properties video ([prop (hash)])
  ()
  (error "TODO"))

(define (get-property dict key
                      [default #f]
                      #:extra-info [extra-info #f])
  (dict-ref (properties-prop dict) key default))

(define-constructor service properties ([filters '()]) ())

(define-constructor filter service ([type #f] [source #f])
  ()
  (error "TODO"))

(define-constructor nullsink service ([source #f])
  ()
  (define node (mk-filter-node (hash 'video (mk-filter "nullsink")
                                     'audio (mk-filter "anullsink"))))
  (add-vertex! (current-render-graph) node)
  (when source
    (define s-node (convert source))
    (add-vertex! (current-render-graph) s-node)
    (add-directed-edge! s-node node 1))
  node)

(define-constructor transition service ([source-track1 (hash)]
                                        [source-track2 (hash)]
                                        [length-track1 0]
                                        [length-track2 0])
  ())

(define-constructor consumer service ([type #f] [target #f])
  ()
  (error "TODO"))

(define-constructor producer service ([type #f]
                                      [source #f])
  ()
  (error "TODO"))

(define-constructor file producer ([path #f]) ()
  (when (not path)
    (error 'file "No path given"))
  (define bundle (file->stream-bundle path))
  (define fctx (stream-bundle-avformat-context bundle))
  (define fstart 0)
  (define fend (avformat-context-duration fctx))
  (define fwidth #f)
  (define fheight #f)
  (define ftime-base #f)
  (define fpix-fmt #f)
  (define fsample-fmt #f)
  (define fsample-rate #f)
  (define count-tab (make-hash))
  (define-syntax-rule (set!/error var val comp)
    (cond [var (unless (comp var val)
                 (error 'file "Incompatible stream values ~a and ~a" var val))]
          [else (set! var val)]))
  (for ([str (stream-bundle-streams bundle)])
    (match str
      [(struct* codec-obj ([codec-context cctx]
                           [type type]))
       (hash-update! count-tab type
                     add1
                     0)
       (match type
         ['video
          (set!/error fwidth (avcodec-context-width cctx) =)
          (set!/error fheight (avcodec-context-height cctx) =)
          (set!/error ftime-base (avcodec-context-time-base cctx) =)
          (set!/error fpix-fmt (avcodec-context-pix-fmt cctx) eq?)]
         ['audio
          (set!/error fsample-fmt (avcodec-context-sample-fmt cctx) eq?)
          (set!/error fsample-rate (avcodec-context-sample-rate cctx) =)]
         [_ (void)])]))
  (define node (mk-source-node bundle
                               #:counts count-tab
                               #:props (hash "start" (or fstart 0)
                                             "end" (or fend 0)
                                             "width" (or fwidth 0)
                                             "height" (or fheight 0)
                                             "fps" (if (and ftime-base (not (= ftime-base 0)))
                                                       (/ 1 ftime-base)
                                                       25)
                                             "pix-fmt" fpix-fmt
                                             "sample-fmt" fsample-fmt
                                             "sample-rate" fsample-rate)))
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
      [(empty? elements) (list (make-blank #:start 0 #:end 1))]
      [(transition? (first elements))
       (list (make-blank #:start 0 #:end (transition-length-track1 (first elements))))]
      [else (list)]))
  (define post-list
    (cond
      [(empty? elements) (list)]
      [(transition? (last elements))
       (list (make-blank #:start 0 #:end (transition-length-track2 (last elements))))]
      [else (list)]))
  (define elements*
    (append pre-list elements post-list))
  ;; Generate nodes and handle transitions
  (define-values (rev-trans-vids rev-trans-nodes start end fps width height counts)
    (for/fold ([prev-vids '()]
               [prev-nodes '()]
               [start 0]
               [end 0]
               [fps 0]
               [width 0]
               [height 0]
               [counts (hash)])
              ([i (in-list elements*)]
               [index (in-naturals)])
      (match i
        [(struct* transition ([source-track1 source-track1]
                              [source-track2 source-track2]
                              [length-track1 length-track1]
                              [length-track2 length-track2]))
         (define track1-modifier (mk-filter-node source-track1
                                                 #:props (dict-update (node-props (car prev-nodes))
                                                                      "end"
                                                                      (λ (end)
                                                                        (- end length-track1)))
                                                 #:counts (node-counts (car prev-nodes))))
         (define prev-vid (car prev-vids))
         (add-vertex! (current-render-graph) track1-modifier)
         (add-directed-edge! (current-render-graph)
                             (car prev-nodes) track1-modifier 1)
         (values (cons i prev-vids)
                 (cons track1-modifier (cdr prev-nodes))
                 start
                 (- end (/ (+ (length-track1 i)
                              (length-track2 i))
                           2))
                 fps
                 width
                 height
                 (hash-union counts (node-counts (car prev-nodes) #:combine max)))]
        [else
         (define pre-node (convert i))
         (define node
           (cond [(and (not (null? prev-vids))
                       (transition? (car prev-vids)))
                  (define source-track2 (transition-source-track2 (car prev-vids)))
                  (define length-track2 (transition-length-track2 (car prev-vids)))
                  (define track2-modifier
                    (mk-filter-node source-track2
                                    #:props (dict-update (node-props pre-node)
                                                         "end"
                                                         (λ (end)
                                                           (- end length-track2)))
                                    #:counts (node-counts pre-node)))
                  (add-vertex! (current-render-graph) track2-modifier)
                  (add-directed-edge! (current-render-graph)
                                      pre-node track2-modifier)
                  track2-modifier]
                 [else pre-node]))
         (define props (node-props node))
         (values (cons i prev-vids)
                 (cons node prev-nodes)
                 start
                 (+ end (- (dict-ref props "end") (dict-ref props "start")))
                 (max fps (dict-ref props "fps" 0))
                 (max width (dict-ref props "width" 0))
                 (max height (dict-ref props "height" 0))
                 (hash-union counts (node-counts pre-node) #:combine max))])))
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
      (let* ([ret (coerce-clip (mk-filter "pad" (hash "width" width
                                                      "height" height))
                               node)]
             [ret (coerce-clip (mk-filter "fps" (hash "fps" fps))
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
      ;(define offset (mk-filter "setpts" (hash "expr" (format "PTS-STARTPTS+~a" time))))
      (define aoffset (mk-filter "asetpts" (hash "expr" "PTS-STARTPTS")))
      ;(define aoffset (mk-filter "asetpts" (hash "expr" (format "PTS-STARTPTS+~a" time))))
      ;(define aoffset (mk-filter "adelay" (hash "delays" time)))
      #;
      (define background-a
        (mk-filter-node (hash 'video (mk-empty-video-filter #:width width
                                                            #:height height
                                                            #:duration (- end start))
                              'audio (mk-empty-audio-filter))
                        #:counts counts))
      #;
      (define background-b
        (mk-filter-node (hash 'video (mk-filter "pad" (hash "width" width
                                                            "height" height)))
                        #:counts counts))
      ;(add-vertex! (current-render-graph) background-a)
      ;(add-vertex! (current-render-graph) background-b)
      ;(add-directed-edge! (current-render-graph) background-a background-b)
      #;
      (define overlay
        (mk-filter-node (hash 'video (mk-filter "overlay")
                              'audio (mk-filter "amix"))
                        #:counts counts))
      ;(add-vertex! (current-render-graph) overlay)
      ;(add-directed-edge! (current-render-graph) background-b overlay 1)
      (define node
        (mk-filter-node (hash 'video offset
                              'audio aoffset)
                        #:props (dict-set*! (dict-copy props)
                                            "start" (+ start time)
                                            "end" (+ end time))
                        #:counts counts))
      (add-vertex! (current-render-graph) node)
      (add-directed-edge! (current-render-graph) n node 1)
      ;(add-directed-edge! (current-render-graph) node overlay 2)
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
                                  "height" height)
                    #:counts counts))
  (add-vertex! (current-render-graph) the-buffer)
  (add-directed-edge! (current-render-graph) the-playlist the-buffer 1)
  the-buffer)

(define-constructor multitrack producer ([tracks '()] [field '()])
  ()
  ;; Make the tracks
  (define nodes (make-hash))
  (for ([t (in-list tracks)]
        [index (in-naturals)])
    (dict-set! nodes t (cons (convert t) index)))
  ;; Start merging tracks together
  (for ([f (in-list field)])
    (match f
      [(struct* field-element ([element element]
                               [track track]
                               [track-2 track-2]))
       (define bundle-pair (dict-ref nodes track))
       (define bundle-pair-2 (dict-ref nodes track-2))
       (define element-node (convert element))
       (dict-set! bundle-pair nodes track (cons element-node (min (cdr bundle-pair)
                                                                  (cdr bundle-pair-2))))
       (dict-set! bundle-pair nodes track-2 (cons element-node (min (cdr bundle-pair)
                                                                    (cdr bundle-pair-2))))
       (add-directed-edge! (current-render-graph) (car bundle-pair) element-node 1)
       (add-directed-edge! (current-render-graph) (car bundle-pair-2) element-node 2)]))
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
      (add-directed-edge! (cdr n-pair) trash 1)))
  ret)

(define-constructor field-element video ([element #f] [track #f] [track-2 #f]) ())
