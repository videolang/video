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
         racket/splicing
         racket/math
         file/convertible
         syntax/parse/define
         (prefix-in file: file/convertible)
         (prefix-in base: racket/base)
         graph
         "render-settings.rkt"
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
                     syntax/parse
                     syntax/parse/lib/function-header))

(define current-render-graph (make-parameter (mk-render-graph)))
(define current-video-directory (make-parameter (current-directory)))
(define current-convert-database (make-parameter #f))

;; A helper function to convert videos to video nodes
;; Video (U Graph #f) -> _node
(define (convert source* [target-prop (hash)] [target-counts (hash)]
                 #:renderer [renderer* #f])
  (struct default ())
  (define source
    (cond [(video? source*) source*]
          [(and (video-convertible? source*)
                (video-instance-convertible? source*))
           source*]
          [(and (current-convert-database)
                (send (current-convert-database) convertible? source*))
           (send (current-convert-database) convert source*)]
          [(file:convertible? source*)
           (let ([ret (file:convert source* 'video (default))])
             (when (default? ret)
               (error 'convert "Object ~a cannot be converted to a video with file api" source*))
             ret)]
          [else (error 'convert "Object ~a cannot be converted to a video" source*)]))
  (parameterize ([current-render-graph (or renderer* (current-render-graph))])
    (define ret (video-convert source target-prop target-counts))
    ret))

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; avformat Path -> Void
(define (debug/save-prop avformat path)
  (av-dump-format avformat 0 path 0))

(define (finish-video-object-init video-node video-source target-prop target-counts)
  ;; Sync target counts
  (define counts-node
    (cond [(not (equal? target-counts (node-counts video-node)))
           (define n (mk-filter-node (hash 'video (mk-filter "fifo")
                                           'audio (mk-filter "afifo"))
                                     #:props (node-props video-node)
                                     #:counts target-counts))
           (add-vertex! (current-render-graph) n)
           (add-directed-edge! (current-render-graph) video-node n 1)
           n]
          [else video-node]))
  ;; Set user properties
  (define vprop
    (cond
      [(properties? video-source)
       (let* ([user-prop (properties-prop video-source)]
              ;; Demuxing (TODO)
              [node
               (cond [(dict-ref user-prop "video-index" #f)
                      counts-node] ;; TODO
                     [(dict-ref user-prop "audio-index" #f)
                      counts-node] ;; TODO
                     [else counts-node])]
              ;; Dimmensions
              [width (or (dict-ref target-prop "width" #f)
                         (dict-ref user-prop "width" #f))]
              [height (or (dict-ref target-prop "height" #f)
                          (dict-ref user-prop "height" #f))]
              [dar (or (dict-ref target-prop "display-aspect-ratio" #f)
                       (dict-ref user-prop "display-aspect-ratio" #f))]
              [prev-props (or (node-props node) (hash))]
              [prev-width (dict-ref prev-props "width" #f)]
              [prev-height (dict-ref prev-props "height" #f)]
              [prev-dar (dict-ref prev-props "dar" #f)]
              [node
               (cond [(or (and width height
                               (not (and prev-width (= prev-width width)))
                               (not (and prev-height (= prev-height height))))
                          (and dar (not (and prev-dar (= prev-dar dar)))))
                      (define s-node
                        (mk-filter-node (hash 'video (mk-filter "scale"
                                                            (hash "width" width
                                                                  "height" height)))
                                        #:props (dict-set* prev-props
                                                           "width" width
                                                           "height" height)
                                        #:counts (node-counts node)))
                      (add-vertex! (current-render-graph) s-node)
                      (add-directed-edge! (current-render-graph) node s-node 1)
                      (define dar-node
                        (mk-filter-node (hash 'video (mk-filter "setdar"
                                                                (hash "r" (racket->ffmpeg dar))))
                                        #:props (dict-set* (node-props s-node)
                                                           "display-aspect-ratio" dar)
                                        #:counts (node-counts s-node)))
                      (add-vertex! (current-render-graph) dar-node)
                      (add-directed-edge! (current-render-graph) s-node dar-node 1)
                      dar-node]
                     [else node])]
              ;; Time trimming
              [calc-start (λ (prop)
                            (or (dict-ref prop "start" #f)
                                (and (dict-ref prop "length" #f) 0)
                                (and (dict-ref prop "end" #f) 0)))]
              [start (or (calc-start target-prop)
                         (calc-start user-prop))]
              [calc-end (λ (prop)
                         (or (dict-ref prop "end" #f)
                             (let ([l (dict-ref prop "length" #f)])
                               (and l (+ start l)))))]
              [end (let ([t-end (calc-end target-prop)]
                         [u-end (calc-end user-prop)])
                     (cond
                       [(not t-end)      u-end]
                       [(= t-end +inf.0) (or u-end t-end)]
                       [else             t-end]))]
              [prev-start (dict-ref prev-props "start" #f)]
              [prev-end (dict-ref prev-props "end" #f)]
              [times-unset? (dict-ref prev-props "times-unset?" #f)]
              [node
               (cond
                 [(or times-unset?
                      (and start end
                           (not (= end +inf.0))
                           (not (and prev-start (= prev-start start)
                                     prev-end (= prev-end end)))))
                  (define n
                    (mk-filter-node (hash 'video (mk-filter "trim"
                                                            (hash "start" (racket->ffmpeg start)
                                                                  "end" (racket->ffmpeg end)))
                                          'audio (mk-filter "atrim"
                                                            (hash "start" (racket->ffmpeg start)
                                                                  "end" (racket->ffmpeg end))))
                                    #:props (dict-set* (node-props node)
                                                       "start" start
                                                       "end" end)
                                    #:counts (node-counts node)))
                  (add-vertex! (current-render-graph) n)
                  (add-directed-edge! (current-render-graph) node n 1)
                  (define pts-n
                    (mk-filter-node (hash 'video (mk-filter "setpts" (hash "expr" "PTS-STARTPTS"))
                                          'audio (mk-filter "asetpts" (hash "expr" "PTS-STARTPTS")))
                                    #:props (node-props n)
                                    #:counts (node-counts n)))
                  (add-vertex! (current-render-graph) pts-n)
                  (add-directed-edge! (current-render-graph) n pts-n 1)
                  pts-n]
                 [else node])])
         node)]
      [else counts-node]))
  ;; Attach filters
  (define attached
    (if (service? video-source)
        (for/fold ([ret vprop])
                  ([f (in-list (service-filters video-source))])
          (define f* (convert-filter f target-prop video-source ret))
          f*)
        vprop))
  ;; Extend properties table (TODO)
  (define extended
    (cond [(properties? attached) attached]
          [else attached]))
  ;; Apply any remaining conversions needed to transform user-prop to target-prop
  ;;   and user counts to target counts
  ;; (height/width + start/end, handled earlier)
  (let* ([node extended]
         [prev-prop (or (node-props node) (hash))]
         ;; FPS
         [target-fps (dict-ref target-prop "fps" #f)]
         [prev-fps (dict-ref prev-prop "fps" #f)]
         [node
          (cond [(and target-fps
                      (not (and prev-fps (= target-fps prev-fps))))
                 (define n
                   (mk-filter-node
                    (hash 'video (mk-filter "fps"
                                            (hash "fps" target-fps )))
                    #:props (dict-set* prev-prop
                                       "fps" target-fps)
                    #:counts (node-counts node)))
                 (add-vertex! (current-render-graph) n)
                 (add-directed-edge! (current-render-graph) node n 1)
                 n]
                [else node])]
         ;; Time base
         [video-target-time-base (dict-ref target-prop "video-time-base" #f)]
         [video-prev-time-base (dict-ref prev-prop "video-time-base" #f)]
         [audio-target-time-base (dict-ref target-prop "audio-time-base" #f)]
         [audio-prev-time-base (dict-ref prev-prop "audio-time-base" #f)]
         [node
          (cond [(or (and video-target-time-base
                          (not (and video-prev-time-base
                                    (= video-target-time-base video-prev-time-base))))
                     (and audio-target-time-base
                          (not (and audio-prev-time-base
                                    (= audio-target-time-base audio-prev-time-base)))))
                 (define tb
                   (mk-filter-node
                    (hash-union
                    (if video-target-time-base
                        (hash 'video (mk-filter "settb" (hash "expr" video-target-time-base)))
                        (hash))
                    (if audio-target-time-base
                        (hash 'audio (mk-filter "asettb" (hash "expr" audio-target-time-base)))
                        (hash)))
                    #:props (dict-set* (node-props node)
                                       "video-time-base" video-target-time-base
                                       "audio-time-base" audio-target-time-base)
                   #:counts (node-counts node)))
                 (add-vertex! (current-render-graph) tb)
                 (add-directed-edge! (current-render-graph) node tb 1)
                 tb]
                [else node])]
         ;; Pix Fmt/Sample Fmt/Chan layout
         [target-pix-fmt (dict-ref target-prop "pix-fmt" #f)]
         [prev-pix-fmt (dict-ref prev-prop "pix-fmt" #f)]
         [target-sample-fmt (dict-ref target-prop "sample-fmt" #f)]
         [prev-sample-fmt (dict-ref prev-prop "sample-fmt" #f)]
         [target-sample-rate (dict-ref target-prop "sample-rate" #f)]
         [prev-sample-rate (dict-ref prev-prop "sample-rate" #f)]
         [target-channel-layout (dict-ref target-prop "channel-layout" #f)]
         [prev-channel-layout (dict-ref prev-prop "channel-layout" #f)]
         [node
          (cond [(or (and target-pix-fmt
                          (not (eq? target-pix-fmt prev-pix-fmt)))
                     (and target-sample-fmt
                          (not (eq? target-sample-fmt prev-sample-fmt)))
                     (and target-sample-rate
                          (not (and prev-sample-rate (= target-sample-rate prev-sample-rate))))
                     (and target-channel-layout
                          (not (eq? target-channel-layout prev-channel-layout))))
                 (define pix-fmt-node
                   (mk-filter-node
                    (hash 'video (mk-filter "format" (hash "pix_fmts" target-pix-fmt))
                          'audio (mk-filter "aformat" (hash "sample_fmts" target-sample-fmt
                                                            "sample_rates" target-sample-rate
                                                            "channel_layouts" target-channel-layout)))
                    #:props (dict-set* (node-props node)
                                       "pix-fmt" target-pix-fmt
                                       "sample-fmt" target-sample-fmt
                                       "sample-rate" target-sample-rate
                                       "channel-layout" target-channel-layout)
                    #:counts (node-counts node)))
                 (add-vertex! (current-render-graph) pix-fmt-node)
                 (add-directed-edge! (current-render-graph) node pix-fmt-node 1)
                 pix-fmt-node]
                [else node])]
         ;; Speed
         [target-speed (dict-ref target-prop "speed" #f)]
         [prev-speed (dict-ref prev-prop "speed" #f)]
         [target-video-codec (dict-ref target-prop "video-codec" #f)]
         [node
          (cond [(and target-speed
                      (not (and prev-speed (= prev-speed target-speed)))
                      (not (= target-speed 0)))
                 (define speed-node
                   (mk-filter-node
                    (hash 'video (mk-filter
                                  "setpts"
                                  (hash "expr" (base:format
                                                "(PTS-STARTPTS)*~a"
                                                (exact->inexact (/ 1 (abs target-speed))))))
                          'audio (mk-filter
                                  "asetrate"
                                  (hash "r" (exact->inexact
                                             (abs (* target-sample-rate target-speed))))))
                    #:props (dict-set* (node-props node)
                                       "speed" target-speed)
                    #:counts (node-counts node)))
                 (add-vertex! (current-render-graph) speed-node)
                 (add-directed-edge! (current-render-graph) node speed-node 1)
                 (define drop-node
                   (match target-video-codec
                     ['mpeg1video
                      (define drop-node
                        (mk-filter-node
                         (hash 'video (mk-filter "select"
                                                 (hash "expr" (format "'not(mod(n\\,~a)'"
                                                                      (exact-ceiling target-speed))))
                               #:props (node-props speed-node)
                               #:counts (node-counts speed-node))))
                      (add-vertex! (current-render-graph) drop-node)
                      (add-directed-edge! (current-render-graph) speed-node drop-node 1)
                      drop-node]
                     [_ speed-node]))
                 (define rev-node
                   (cond
                     [(< target-speed 0)
                      (define rev-node
                        (mk-filter-node
                         (hash 'video (mk-filter "reverse")
                               'audio (mk-filter "areverse"))
                         #:props (node-props drop-node)
                         #:counts (node-counts drop-node)))
                      (add-vertex! (current-render-graph) rev-node)
                      (add-directed-edge! (current-render-graph) drop-node rev-node 1)
                      rev-node]
                     [else drop-node]))
                 rev-node]
                [else node])])
    node))

;; Having `convertible?` be generic allows any struct _instance_
;;   to specify that its not convertible, even if the struct _type_
;;   usually is.
(define-generics video-convertible
  #:fallbacks [(define (video-instance-convertible? v) #t)]
  (video-instance-convertible? video-convertible)
  (video-convert video-convertible [target-prop] [target-counts]))

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
     #:with the-target-prop (format-id stx "target-prop")
     #:with the-target-counts (format-id stx "target-counts")
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
           #:methods gen:video-convertible
           [(define (video-convert v
                                   [target-prop (hash)]
                                   [target-counts (hash)])
              (convert-name v target-prop target-counts))]
           #:property prop:convertible
           (λ (v request def)
             (match request
               ['video
                (convert-name v (hash) (hash))]
               [_ def])))
         #,(quasisyntax/loc stx
             (define (convert-name v [p (hash)] [tc (hash)] [convert-args convert-defaults] ...)
               (define ret
                 (call-with-values
                  (λ ()
                    (let ([this v]
                          [the-target-prop p]
                          [the-target-counts tc])
                      (let #,(for/list ([i (in-list all-structs)]
                                        [j (in-list all-ids)])
                               #`[#,(datum->syntax stx j)
                                  (#,(format-id stx "~a-~a" i j) v)])
                        #f body ...)))
                  list))
               (define finished
                 (for/list ([i (in-list ret)])
                   (and i
                        (finish-video-object-init i v p tc))))
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
  (cond
    [(properties? obj)        ;; Already a video
     (define new-props (hash-set (properties-prop obj) key val))
     (copy-video obj #:prop new-props)]
    [(video-convertible? obj) ;; Easily convertible to a video
     (convert obj (hash key val) (hash))]))
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

(define-constructor filter service ([subgraph #f]
                                    [source-props-proc #f])
  ([prev-source #f] ; <- Source for video
   [prev-node #f]) ; <- Compiled form of prev
  (unless prev-node
    (error 'filter "Prev node ~a not found" prev-node))
  (cond
    [(dict? subgraph)
     (define node (mk-filter-node subgraph
                                  #:counts (node-counts prev-node)
                                  #:props (node-props prev-node)))
     (add-vertex! (current-render-graph) node)
     (add-directed-edge! (current-render-graph) prev-node node 1)
     node]
    [(procedure? subgraph)
     (define rg (optional-apply subgraph
                                #:empty-graph (weighted-graph/directed '())
                                #:source-props (node-props prev-node)
                                #:source-counts (node-counts prev-node)
                                #:target-props target-prop
                                #:target-coutns target-counts))
     (graph-union! (current-render-graph) (video-subgraph-graph rg))
     (add-directed-edge! (current-render-graph) prev-node (video-subgraph-sources rg) 1)
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

;; We want to supply keywords for only the ones
;;  the plugin expected.
(define optional-apply
  (make-keyword-procedure
   (λ (kws args . rest)
     (unless (= 1 (length rest))
       (raise-arguments-error 'optional-apply "Invalid argument list ~a" rest))
     (define proc (car rest))
     (match-define-values (_ accepted-kws) (procedure-keywords proc))
     (define-values (kws-to-apply args-to-apply)
       (cond
         [accepted-kws
          (for/fold ([res-k '()]
                     [res-a '()]
                     #:result (values (reverse res-k)
                                      (reverse res-a)))
                    ([k (in-list kws)]
                     [a (in-list args)]
                     #:when #t
                     [ak (in-list accepted-kws)]
                     #:when (equal? k ak))
            (values (cons k res-k) (cons a res-a)))]
         [else (values kws args)]))
     (keyword-apply proc kws-to-apply args-to-apply '()))))

(define-constructor transition service ([source-props-proc #f]
                                        [track1-subgraph #f]
                                        [track2-subgraph #f]
                                        [combined-subgraph #f])
  ([prev1 #f]
   [prev2 #f])
  (unless (and prev1 prev2)
    (error 'transition "Expected prev nodes, found ~a and ~a" prev1 prev2))
  ;; First give filter a chance to set required prev-props based
  ;;    on requested target prop
  (define-values (track1-props track1-counts track2-props track2-counts)
    (optional-apply source-props-proc
                    #:target-props target-prop
                    #:target-counts target-counts))
  ;; Next, compile source nodese given the new target props
  ;; If no new props given, use previous target props
  (define prev1-node (video-convert prev1
                                    (hash-union (or track1-props (hash))
                                                target-prop
                                                #:combine (λ (user targ) user))
                                    (hash-union (or track1-counts (hash))
                                                target-counts
                                                #:combine (λ (user targ) user))))
  (define prev2-node (video-convert prev2
                                    (hash-union (or track2-props (hash))
                                                target-prop
                                                #:combine (λ (user targ) user))
                                    (hash-union (or track2-counts (hash))
                                                target-counts
                                                #:combine (λ (user targ) user))))
  ;; Next, build subgraphs with the newly constructed properties
  (define track1-sub (optional-apply track1-subgraph
                                     #:empty-graph (weighted-graph/directed '())
                                     #:track1-props (node-props prev1-node)
                                     #:track1-counts (node-counts prev1-node)
                                     #:target-props target-prop
                                     #:target-counts target-counts))
  (define track2-sub (optional-apply track2-subgraph
                                     #:empty-graph (weighted-graph/directed '())
                                     #:track2-props (node-props prev2-node)
                                     #:track2-counts (node-counts prev2-node)
                                     #:target-props target-prop
                                     #:target-counts target-counts))
  (define combined-sub
    (optional-apply combined-subgraph
                    #:empty-graph (weighted-graph/directed '())
                    #:track1-props (node-props prev1-node)
                    #:track1-counts (node-counts prev1-node)
                    #:track2-props (node-props prev2-node)
                    #:track2-counts (node-counts prev2-node)
                    #:target-props target-prop
                    #:target-counts target-counts))
  ;; Only copy if needed both by combined and either track1 or track2
  (define prev1-copy
    (cond [(and track1-sub combined-sub)
           (define prev1-copy
             (mk-split-node #:fan-out 2
                            #:counts (node-counts prev1-node)
                            #:props (node-props prev1-node)))
           (add-vertex! (current-render-graph) prev1-copy)
           (add-directed-edge! (current-render-graph) prev1-node prev1-copy 1)
           prev1-copy]
          [else prev1-node]))
  (define prev2-copy
    (cond [(and track2-sub combined-sub)
           (define prev2-copy
             (mk-split-node #:fan-out 2
                            #:counts (node-counts prev2-node)
                            #:props (node-props prev2-node)))
           (add-vertex! (current-render-graph) prev2-copy)
           (add-directed-edge! (current-render-graph) prev2-node prev2-copy 1)
           prev2-copy]
          [else prev2-node]))
  (define r1
    (cond [track1-sub
           (graph-union! (current-render-graph) (video-subgraph-graph track1-sub))
           (add-directed-edge! (current-render-graph)
                               prev1-copy
                               (video-subgraph-sources track1-sub)
                               2)
           (video-subgraph-sinks track1-sub)]
          [else #f]))
  (define r2
    (cond [track2-sub
           (graph-union! (current-render-graph) (video-subgraph-graph track2-sub))
           (add-directed-edge! (current-render-graph)
                               prev2-copy
                               (video-subgraph-sources track2-sub)
                               1)
           (video-subgraph-sinks track2-sub)]
          [else #f]))
  (define rc
    (cond [combined-sub
           (graph-union! (current-render-graph) (video-subgraph-graph combined-sub))
           (add-directed-edge! (current-render-graph)
                               prev1-copy (car (video-subgraph-sources combined-sub)) 1)
           (add-directed-edge! (current-render-graph)
                               prev2-copy (cdr (video-subgraph-sources combined-sub)) 2)
           (video-subgraph-sinks combined-sub)]
          [else #f]))
  (values r1 r2 rc))

(define-constructor producer service ([subgraph (hash)])
  ()
  (cond
    [(dict? subgraph)
     (define node (mk-filter-node subgraph
                                  #:counts (for/hash ([(k v) (in-dict subgraph)])
                                             (values k 1))
                                  #:props prop))
     (add-vertex! (current-render-graph) node)
     node]
    [(procedure? subgraph)
     ;; Compile subgraph
     (define rg
       (optional-apply subgraph
                       #:empty-graph (weighted-graph/directed '())
                       #:target-props target-prop
                       #:target-counts target-counts))
     (graph-union! (current-render-graph) (video-subgraph-graph rg))
     (video-subgraph-sinks rg)]))

(define-constructor file producer ([path #f]) ()
  (when (not path)
    (error 'file "No path given"))
  ;; First get properties of file
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
  (let ()
    (define start (avformat-context-start-time fctx))
    (define duration (avformat-context-duration fctx))
    (unless (= start AV-NOPTS-VALUE)
      (set! ftime-base AV-TIME-BASE-Q)
      (set! fstart start))
    (unless (= duration AV-NOPTS-VALUE)
      (set! ftime-base AV-TIME-BASE-Q)
      (set! fduration duration)))
  (for ([str (stream-bundle-streams bundle)])
    (match str
      [(struct* codec-obj ([codec-context cctx]
                           [type type]
                           [stream stream]))
       (hash-update! count-tab type
                     add1
                     0)
       (when (or (eq? type 'video)
                 (eq? type 'audio))
         (unless ftime-base
           (set!/error ftime-base (avstream-time-base stream) =))
         (unless fstart
           (define avst (avstream-start-time stream))
           (set!/error fstart
                       (if (= avst AV-NOPTS-VALUE) #f avst)
                       =))
         (unless fduration
           (define avd (avstream-duration stream))
           (set!/error fduration
                       (if (= avd AV-NOPTS-VALUE) #f avd)
                       =)))
       (match type
         ['video
          (set!/error fwidth (avcodec-context-width cctx) =)
          (set!/error fheight (avcodec-context-height cctx) =)
          (set!/error ffps (avcodec-context-framerate cctx) =)
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
  (cond
    [(and fstart fduration ftime-base) ; A moving image
     node]
    [else                              ; A still image
     ;; This is terrible, but see:
     ;; video.stackexchange.com/questions/12105/add-an-image-overlay-in-front-of-video-using-ffmpeg
     (define b-node (video-convert (make-blank)
                                   (hash "width" fwidth
                                         "height" fheight)
                                   count-tab))
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
                                           [audio #f]
                                           [render-settings (make-render-settings)])
  ()
  (define devices (list-input-devices))
  (define (make-devstr devlist-op name)
    (and name
         (match (system-type 'os)
           ['macosx (index-of (devlist-op devices) name)]
           ['unix name]
           ['windows name]
           [_ (error 'input-device "Not yet implemented for this platform")])))
  (define vid-str (make-devstr input-devices-video video))
  (define aud-str (make-devstr input-devices-audio audio))
  (define bundle (devices->stream-bundle vid-str aud-str render-settings))
  (define node (mk-source-node bundle
                               #:props (hash "start" 0
                                             "end" +inf.0)
                               #:counts (hash 'video (if vid-str 1 0)
                                              'audio (if aud-str 1 0))))
  (add-vertex! (current-render-graph) node)
  node)

(define-constructor blank producer () ()
  (define start (or (dict-ref target-prop "start" #f)
                    (dict-ref prop "start" #f)))
  (define end (or (dict-ref target-prop "end" #f)
                  (dict-ref prop "end" #f)))
  (define width (dict-ref target-prop "width" #f))
  (define height (dict-ref target-prop "height" #f))
  (mk-filter-node (hash 'video (mk-empty-video-filter #:width width
                                                      #:height height
                                                      #:duration (and end start (- end start)))
                        'audio (mk-empty-audio-filter))
                  #:counts target-counts
                  #:props (dict-set* prop
                                     "start" start
                                     "end" end
                                     "width" width
                                     "height" height)))

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
  (define timeless-target-prop
    (dict-remove (dict-remove target-prop "start") "end"))
  ;; Generate nodes and handle transitions
  ;; resulting nodes can be concatinated directly
  ;; Otherwise just add the compiled filter to the list.
  ;; Invariant assumed: Transitions are never the first or last filters in a list.
  ;; Invariant assumed: No two transitions happen side by side.
  (define-values (nodes len)
    (let loop ([nodes '()]
               [len 0]
               ; [chapters '()] <- TODO!!!
               [elements elements*])
      (cond
        [(empty? elements) (values (reverse nodes)
                                   len)]
        [else
         (define track1 (first elements))
         (cond
           [(or (empty? (rest elements))               ;; Last element
                (not (transition? (second elements)))) ;; Default transition
            (define node (video-convert track1 timeless-target-prop target-counts))
            (add-vertex! (current-render-graph) node)
            (define end (dict-ref (node-props node) "end" #f))
            (define start (dict-ref (node-props node) "start" #f))
            (loop (cons node nodes)
                  (+ len (if (and start end)
                             (- end start)
                             0))
                  (cdr elements))]
           [else                                       ;; User specified transition
            (define trans (second elements))
            (define track2 (third elements))
            (define-values (r1 r2 rc)
              (convert-transition trans timeless-target-prop target-counts track1 track2))
            (define r (append (if r2 (list r1) (list))  ;; <- Backwards
                              (if rc (list rc) (list))
                              (if r1 (list r2) (list))))
            (when r1 (add-vertex! (current-render-graph) r1))
            (define r1end   (and r1 (dict-ref (node-props r1) "end" #f)))
            (define r1start (and r1 (dict-ref (node-props r1) "start" #f)))
            (when r2 (add-vertex! (current-render-graph) r2))
            (define r2end   (and r2 (dict-ref (node-props r2) "end" #f)))
            (define r2start (and r2 (dict-ref (node-props r2) "start" #f)))
            (when rc (add-vertex! (current-render-graph) rc))
            (define rcend   (and rc (dict-ref (node-props rc) "end" #f)))
            (define rcstart (and rc (dict-ref (node-props rc) "start" #f)))
            (loop (append r nodes)
                  (+ len
                     (if (and r1end r1start)
                         (- r1end r1start)
                         0)
                     (if (and r2end r2start)
                         (- r2end r2start)
                         0)
                     (if (and rcend rcstart)
                         (- rcend rcstart)
                         0))
                  (list-tail elements 3))])])))
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
                    #:props (dict-set* target-prop
                                       "video-time-base" AV-TIME-BASE-Q
                                       "audio-time-base" AV-TIME-BASE-Q
                                       "start" 0
                                       "end" len)
                    #:counts target-counts))
  (add-vertex! (current-render-graph) the-playlist)
  (for ([i (in-list nodes)]
        [index (in-naturals)])
    (define pre-buffer (mk-filter-node (hash 'video (mk-filter "fifo")
                                             'audio (mk-filter "afifo"))
                                       #:props (node-props i)
                                       #:counts target-counts))
    (add-vertex! (current-render-graph) pre-buffer)
    (add-directed-edge! (current-render-graph) i pre-buffer 1)
    (add-directed-edge! (current-render-graph) pre-buffer the-playlist index))
  #|
  ;; buffer the results (not needed?)
  (define the-buffer
    (mk-filter-node (hash 'video (mk-filter "fifo") 'audio (mk-filter "afifo"))
                    #:props (dict-set* target-prop
                                       "start" 0
                                       "end" len)
                    #:counts counts))
  (add-vertex! (current-render-graph) the-buffer)
  (add-directed-edge! (current-render-graph) the-playlist the-buffer 1)
  the-buffer
|#
  the-playlist)

(define-constructor multitrack producer ([tracks '()] [field '()])
  ()
  (define (find-length r)
    (cond
      [r
       (define a (dict-ref (node-props r) "end" #f))
       (define b (dict-ref (node-props r) "start" #f))
       (if (and a b) (- a b) +inf.0)]
      [else +inf.0]))
  ;; Generate nodes and handle transitions
  ;; resulting nodes can be concatinated directly
  ;; Otherwise just add the compiled filter to the list.
  ;; Invariant assumed: Transitions are never the first or last filters in a list.
  ;; Invariant assumed: No two transitions happen side by side.
  (define-values (nodes len)
    (let loop ([nodes '()]
               [len +inf.0]
               ; [chapters '()] <- TODO!!!
               [elements tracks])
      (cond
        [(empty? elements) (values (reverse nodes)
                                   (and (= len +inf.0) len))]
        [else
         (define track1 (first elements))
         (cond
           [(or (empty? (rest elements))               ;; Last element
                (not (transition? (second elements)))) ;; Default transition
            (define node (video-convert track1 target-prop target-counts))
            (add-vertex! (current-render-graph) node)
            (loop (cons node nodes)
                  (+ len (- (dict-ref (node-props node) "end" 0)
                            (dict-ref (node-props node) "start" 0)))
                  (cdr elements))]
           [else                                       ;; User specified transition
            (define trans (second elements))
            (define track2 (third elements))
            (define-values (r1 r2 rc)
              (convert-transition trans target-prop target-counts track1 track2))
            (define r (append (if r2 (list r1) (list))  ;; <- Backwards
                              (if rc (list rc) (list))
                              (if r1 (list r2) (list))))
            (when r1 (add-vertex! (current-render-graph) r1))
            (when r2 (add-vertex! (current-render-graph) r2))
            (when rc (add-vertex! (current-render-graph) rc))
            (loop (append r nodes)
                  (min len
                       (find-length r1)
                       (find-length r2)
                       (find-length rc))
                  (list-tail elements 3))])])))
  ;; Pick top remaining node, discard the rest.
  ;; Return top-most track, throw away rest
  (for ([i (in-list (cdr nodes))])
    (define trash (convert (make-nullsink)))
    (add-vertex! (current-render-graph) trash)
    (add-directed-edge! (current-render-graph) i trash 1))
  (car nodes))

(define-constructor video-subgraph properties ([graph (mk-render-graph)]
                                               [sinks '()]
                                               [sources '()])
  ()
  (graph-union! (current-render-graph) graph)
  (car sources))

(define-constructor field-element video ([element #f] [track #f] [track-2 #f]) ())
