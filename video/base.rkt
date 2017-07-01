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

(require racket/contract/base
         racket/class
         racket/draw
         racket/match
         racket/format
         racket/list
         racket/set
         racket/math
         racket/dict
         syntax/location
         (except-in pict frame blank)
         graph
         "private/video.rkt"
         "surface.rkt"
         (prefix-in core: "private/video.rkt")
         "units.rkt"
         (for-syntax syntax/parse
                     racket/base
                     racket/syntax))

(provide
 (contract-out
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (->* []
                   [#:transitions (listof field-element?)
                    #:properties (or/c (hash/c string? any/c) #f)
                    #:filters (or/c (listof filter?) #f)]
                   #:rest (listof any/c)
                   producer?)]

  ;; Creates a playlist (tracks playing in sequence)
  ;;   (syntactic sugar for list)
  [playlist (->* []
                 [#:transitions (listof field-element?)
                  #:properties (or/c (hash/c string? any/c) #f)
                  #:filters (or/c (listof filter?) #f)]
                 #:rest (listof any/c)
                 producer?)]

  ;; Creates a blank video, for offsetting
  ;;  clips in a playlist
  [blank (-> (or/c nonnegative-integer? #f) (or/c blank? producer?))]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->producer [(or/c path-string? path?)]
                    [])]

  ;; Creates a producer that is a solid color
  [color (->producer [(or/c string? (is-a?/c color%)
                            (list/c byte? byte? byte?))]
                     [])]

  ;; Create a producer that is the same as the other producer but with one or more
  ;; filters attached to it
  [attach-filter (-> service? filter? ... producer?)]

  ;; Creates a clip who's producer is path
  [image (->producer [(or/c path-string? path-for-some-system?)]
                     [])]

  ;; Creates a fading transition from a start to end clip
  [fade-transition (->transition [] []
                                 #:direction s/e)]

  ;; Creates a composite transition where the top track is
  ;;   placed above the bottom track
  #;[composite-transition (->transition [(or/c (between/c 0 1) pixels?)
                                       (or/c (between/c 0 1) pixels?)
                                       (or/c (between/c 0 1) pixels?)
                                       (or/c (between/c 0 1) pixels?)]
                                       []
                                       #:direction t/b)]

  ;; Creates a swiping transition from a start clip to an
  ;;   end clip
  [swipe-transition (->transition [#:direction symbol?]
                                  []
                                  #:direction t/b)]
  
  [scale-filter (-> (and/c number? positive?) (and/c number? positive?) filter?)]

  [grayscale-filter (-> filter?)]

  ;; Set a property associated with a properties struct
  [set-property (-> properties? string? any/c properties?)]

  ;; Get a property associated with a properties struct
  [get-property (->* [properties? string?]
                     [symbol?]
                     any/c)]

  ;; Generate a new video with a new in-out
  [cut-producer (->* [producer?]
                     [#:start (or/c nonnegative-integer? #f)
                      #:end (or/c nonnegative-integer? #f)]
                     producer?)]

  ;; Envelope filter for audio tracks
  [envelope-filter (-> #:length nonnegative-integer?
                       #:direction (or/c 'in 'out)
                       filter?)])

  external-video)

(define (blank length)
  (if length
      (make-blank #:prop (hash "start" 0
                               "end" length))
      (color "black")))

(define (clip path
              #:filters [filters #f]
              #:properties [properties #f])
  (define clip-path (relative-path->string path))
  (make-file #:path clip-path
             #:prop (or properties (hash))
             #:filters (or filters '())))

(define (image path
               #:filters [filters #f]
               #:properties [properties #f])
  (clip path #:filters filters #:properties properties))

(define (color c
               #:filters [filters #f]
               #:properties [properties #f])
  (define c*
    (match c
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% c)]))
  (clip (format "color:0x~a~a~a~a"
                (number->2string (send c* red))
                (number->2string (send c* green))
                (number->2string (send c* blue))
                (number->2string (inexact->exact (round (* 255 (send c* alpha))))))
        #:filters [filters #f]
        #:properties [properties #f]))

(define (multitrack #:transitions [transitions '()]
                    #:properties [prop #f]
                    #:filters [maybe-filters #f]
                    . tracks)
  (for ([t (in-list transitions)])
    (define start (field-element-track t))
    (define end (field-element-track-2 t))
    (unless (or start end)
      (error 'multitrack "Both starting and ending clips are #f"))
    (unless (or (not start) (set-member? tracks start))
      (error 'multitrack "Starting clip ~a not found: ~a" start playlist))
    (unless (or (not end) (set-member? tracks end))
      (error 'multitrack "Ending clip ~a not found: ~a" end playlist)))
  (match-define-values (tracks* transitions* _)
    (for/fold ([tracks* '()]
               [transitions* transitions]
               [prev #f])
              ([track (in-list tracks)]
               [i (in-naturals)])
      (cond
        [(transition? track) (values tracks*
                                     (cons (make-field-element #:element track
                                                               #:track prev
                                                               #:track-2 (list-ref tracks (add1 i)))
                                           transitions*)
                                     prev)]
        [else (values (cons track tracks*) transitions*
                      (if (or (= i 0)
                              (not (transition? (list-ref tracks (sub1 i)))))
                          track
                          prev))])))
  (make-multitrack #:tracks (reverse tracks*)
                   #:field transitions*
                   #:prop (or prop (hash))
                   #:filters (or maybe-filters '())))

(define (playlist #:transitions [transitions '()]
                  #:properties [prop #f]
                  #:filters [maybe-filters #f]
                  . clips)
  (make-playlist
   #:elements
   (for/fold ([acc clips])
             ([t (in-list transitions)])
     (define start (field-element-track t))
     (define end (field-element-track-2 t))
     (unless (or start end)
       (error playlist "Both starting and ending clips are #f"))
     (unless (or (not start) (set-member? clips start))
       (error 'playlist "Starting clip ~a not found: ~a" start playlist))
     (unless (or (not end) (set-member? clips end))
       (error 'playlist "Ending clip ~a not found: ~a" end playlist))
     (append*
      (for/list ([clip (in-list clips)])
        (cond
          [(equal? start clip)
           (list clip (field-element-element t))]
          [(and (not start) (equal? end clip))
           (list (field-element-element t) clip)]
          [else (list clip)]))))
   #:prop (or prop (hash))
   #:filters (or maybe-filters '())))

(define (attach-filter obj . f)
  (define new-filters (append f (service-filters obj)))
  (copy-video obj #:filters new-filters))

(define (set-property obj key val)
  (define new-props (hash-set (properties-prop obj) key val))
  (copy-video obj #:prop new-props))

(define-transition (fade-transition #:length [fade-length 1])
  #:direction s/e
  #:track1-subgraph (λ (ctx node-a)
                      #f)
  #:track2-subgraph (λ (ctx node-b)
                      #f)
  #:combined-subgraph (λ (ctx node-a node-b)
                        (define width (max (get-property node-a "width")
                                           (get-property node-b "width")))
                        (define height (max (get-property node-a "height")
                                            (get-property node-b "height")))
                        (define len-a (- (get-property node-a "end") (get-property node-a "start")))
                        (define len-b (- (get-property node-b "end") (get-property node-b "start")))
                        (define t-length (- (+ len-a len-b) fade-length))
                        (define bg-node
                          (mk-filter-node
                           (hash 'video (mk-filter "color" (hash "color" "black"
                                                                 "size" (format "~ax~a" width height)
                                                                 "d" t-length))
                                 'audio (mk-empty-audio-filter))
                           #:counts (node-counts node-a)))
                        (define pad-a
                          (mk-filter-node (hash 'video "pad" (hash "width" width
                                                                   "height" height))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pad-a)
                        (define pad-b
                          (mk-filter-node (hash 'video (mk-filter "pad" (hash "width" width
                                                                              "height" height)))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pad-b)
                        (define pts-a
                          (mk-filter-node (hash 'video (mk-filter "setpts"
                                                                  (hash "expr" "PTS-STARTPTS")))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pts-a)
                        (add-directed-edge! ctx pad-a pts-a 1)
                        (define pts-b
                          (mk-filter-node
                           (hash 'video (mk-filter "setpts"
                                                   (hash "expr" (format "PTS-STARTPTS+(~a/TB)"
                                                                        (- len-a fade-length)))))
                           #:counts (node-counts node-b)))
                        (add-vertex! ctx pts-b)
                        (add-directed-edge! ctx pad-b pts-b 1)
                        (define ovr-a
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "amix"
                                                                  (hash "inputs" 2
                                                                        "duration" "shortest")))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx ovr-a)
                        (add-directed-edge! ctx bg-node ovr-a 1)
                        (add-directed-edge! ctx pts-a ovr-a 2)
                        (define ovr-b
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "acrossfade"
                                                                  (hash "d" fade-length)))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx ovr-b)
                        (add-directed-edge! ctx ovr-a ovr-b 1)
                        (add-directed-edge! ctx pts-b ovr-b 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons pad-a pad-b)
                                             #:sinks ovr-b)))

#; ;TODO
(define-transition (composite-transition x y w h)
  #:direction t/b
  #:type 'composite
  #:prod-1 back
  #:prod-2 front
  #:source (format "~a%/~a%:~a%x~a%"
                   (unit-dispatch back "width" x)
                   (unit-dispatch back "height" y)
                   (unit-dispatch front "width" w)
                   (unit-dispatch front "height" h)))

(define-transition (swipe-transition #:direction dir)
  #:direction t/b
  (error "TODO"))

(define (scale-filter w h)
  (make-filter #:type 'frei0r.scale0tilt
               #:prop (hash "4" w "5" h)))

;(define audio-fade-filter

(define-syntax (external-video stx)
  (syntax-parse stx
    [(_ mod
        (~or (~optional (~seq #:start start*) #:defaults ([start* #'#f]))
             (~optional (~seq #:end end*) #:defaults ([end* #'#f]))
             (~optional (~seq #:length length) #:defaults ([length #'#f])))
        ...)
     #'(let ()
         (define start (or start* (and length 0)))
         (define end (or end* length))
         (let* ([vid (dynamic-require
                      (module-path-index-join
                       mod
                       (variable-reference->module-path-index (#%variable-reference)))
                      'vid)]
                [vid (if start (set-property vid "start" start) vid)]
                [vid (if end (set-property vid "end" end) vid)])
           vid))]))

(define (cut-producer producer
                      #:start [start #f]
                      #:end [end #f])
  (copy-video producer
              #:prop (let* ([p (properties-prop producer)]
                            [p (if start (dict-set p "start" start) p)]
                            [p (if end (dict-set p "end" end) p)])
                       p)))

(define (grayscale-filter)
  (error "TODO"))

(define (envelope-filter #:direction direction
                         #:length length)
  (error "TODO"))

;; ===================================================================================================
;; Helpers used by this module (not provided)
;; ===================================================================================================

;; Converts a number to a 2 character octal string
;; Number -> String
;; Given: 2 Expect: "02"
;; Given: 255 Expect: "ff"
(define (number->2string number)
  (~a #:left-pad-string "0"
      #:min-width 2
      #:max-width 2
      #:align 'right
      (format "~x" number)))

;; Convert a path relative to a video file to
;; a string that is the absolute path.
;; Path -> String
(define (relative-path->string path)
  (path->string (path->complete-path path (current-video-directory))))

;; Dispatch on the type of value given to it
;; any/c -> Nonnegative-Integer
(define (unit-dispatch prod prop val)
  val)
#|
  (cond
    [(pixel? val)
     (* (pixel-val val) (get-property prod prop))]
    [else
     (inexact->exact (round (* val 100)))]))
|#
