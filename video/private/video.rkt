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

(define current-render-graph (make-parameter (weighted-graph/directed '())))

;; A helper function to convert videos to MLT object
;; Video (U Renderer% #f) -> _mlt-object
(define (convert source
                 #:renderer [renderer* #f])
  (error "TODO"))

;; Helper function to determine if a producer
;; can be potentially unbounded in length.
;; Producer -> Boolean
(define (unbounded-video? prod)
  (cond
    [(playlist? prod) (ormap unbounded-video? (playlist-elements prod))]
    [(multitrack? prod) (andmap unbounded-video? (multitrack-tracks prod))]
    [(producer? prod) (producer-unbounded? prod)]
    [else #f])) ;; Should only happen in playlists

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; Properties Path -> Void
(define (debug/save-prop prop filepath)
  (error "TODO"))

(define (finish-video-object-init! video-object video)
  ;; Set properties
  (when (properties? video)
    (for ([(k* v*) (in-dict (properties-prop video))])
      (error "TODO")))
  ;; Attach filters
  (when (service? video)
    (for ([f (in-list (service-filters video))])
      (error "TODO"))))

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
                 ['mlt
                  (if #f ;(current-skip-memoize?)
                      (convert-name)
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
           (when ret
             (finish-video-object-init! ret v))
           ret)
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
                      [extra-info #f])
  (error "TODO"))

(define-constructor service properties ([filters '()]) ())

(define-constructor filter service ([type #f] [source #f])
  ()
  (error "TODO"))

(define-constructor transition service ([source-track1 (hash)]
                                        [source-track2 (hash)]
                                        [length-track1 0]
                                        [length-track2 0])
  ())

(define-constructor consumer service ([type #f] [target #f])
  ()
  (error "TODO"))

(define-constructor producer service ([type #f]
                                      [source #f]
                                      [start 0]
                                      [end 0]
                                      [speed #f]
                                      [seek #f]
                                      [x #f]
                                      [y #f]
                                      [width #f]
                                      [height #f]
                                      [unbounded? #f])
  ()
  (error "TODO"))

(define-constructor file producer ([file #f])
  ()
  (define bundle (file->stream-bundle file))
  (define avformat (stream-bundle-avformat-context bundle))
  (define video-stream (maybe-av-find-best-stream avformat 'video))
  (define audio-stream (maybe-av-find-best-stream avformat 'audio))
  (define subtitle-stream (maybe-av-find-best-stream avformat 'subtitle))
  (error "TODO, add streams")
  (source-node bundle))

(define-constructor blank producer () ()
  (error "TODO"))

(define-constructor playlist producer ([elements '()])
  ()
  (define pre-list
    (cond
      [(empty? elements) (list (blank #:start 0 #:end 1))]
      [(transition? (first elements))
       (list (blank #:start 0 #:end (transition-length-track1 (first elements))))]
      [else (list)]))
  (define post-list
    (cond
      [(empty? elements) (list)]
      [(transition? (last elements))
       (list (blank #:start 0 #:end (transition-length-track2 (last elements))))]
      [else (list)]))
  (define elements*
    (append pre-list elements post-list))
  ;; Generate nodes and handle transitions
  (define-values (rev-trans-vids rev-trans-nodes start end fps width height)
    (for/fold ([prev-vids '()]
               [prev-nodes '()]
               [start 0]
               [end 0]
               [fps 0]
               [width 0]
               [height 0])
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
                                                                        (- end length-track1)))))
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
                 height)]
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
                                                           (- end length-track2)))))
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
                 (max height (dict-ref props "height" 0)))])))
  (define transition-nodes (reverse rev-trans-nodes))
  (define transition-videos (reverse rev-trans-vids))
  ;; Remove now unneded transitions
  (define only-videos (filter (compose not transition?) transition-videos))
  ;; Go through again and fix frame rates and aspect ratios
  (define-values (rev-cleaned-vids rev-cleaned-nodes)
    (for/fold ([prev-nodes '()])
              ([node (in-list transition-nodes)]
               [vid (in-list only-videos)])
      (cons node prev-nodes)))
  (define cleaned-nodes (reverse rev-cleaned-nodes))
  (log-warning "Need to fix FPS/Width/Height")
  ;; Offset each clip accordingly
  (define-values (prev-nodes time-again)
    (for/fold ([prev-nodes '()]
               [time 0])
              ([n (in-list cleaned-nodes)])
      (define props (node-props n))
      (define start (dict-ref props "start"))
      (define end (dict-ref props "end"))
      (define offset-str (format "setpts=PTS-STARTPTS+~a" time))
      (define node
        (mk-filter-node (hash 'video offset-str
                              'audio offset-str
                              'subtitle offset-str)
                        #:props (dict-set* props
                                           "start" (+ start time)
                                           "end" (+ end time))))
      (add-vertex! (current-render-graph) node)
      (add-directed-edge! (current-render-graph) n node 1)
      (values (cons node prev-nodes) (+ time (- end start)))))
  (define nodes (reverse prev-nodes))
  ;; Build backing structure and transition everything to it.
  (define background (blank #:start start
                            #:end end
                            #:fps fps
                            #:width width
                            #:height height))
  (for/fold ([curr-back background])
            ([i nodes]
             [index (in-naturals)])
    (define new-background
      (mk-filter-node #:video "overlay"
                      #:props (hash "start" start
                                    "end" end
                                    "fps" fps
                                    "width" width
                                    "height" height)))
    (add-vertex! (current-render-graph) new-background)
    (add-directed-edge! (current-render-graph) curr-back new-background 1)
    (add-directed-edge! (current-render-graph) i new-background 2)
    new-background))

(define-constructor multitrack producer ([tracks '()] [field '()])
  ()
  (error "TODO"))

(define-constructor field-element video ([element #f] [track #f] [track-2 #f]) ())
