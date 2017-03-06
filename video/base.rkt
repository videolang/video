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
         syntax/location
         (except-in pict frame blank)
         "private/video.rkt"
         "private/utils.rkt"
         "private/surface.rkt"
         (prefix-in core: "private/video.rkt")
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
                    #:start (or/c nonnegative-integer? #f)
                    #:end (or/c nonnegative-integer? #f)
                    #:length (or/c nonnegative-integer? #f)
                    #:prop (or/c (hash/c string? any/c) #f)
                    #:filters (or/c (listof filter?) #f)]
                   #:rest (listof any/c)
                   producer?)]

  ;; Creates a playlist (tracks playing in sequence)
  ;;   (syntactic sugar for list)
  [playlist (->* []
                 [#:transitions (listof field-element?)
                  #:start (or/c nonnegative-integer? #f)
                  #:end (or/c nonnegative-integer? #f)
                  #:length (or/c nonnegative-integer? #f)
                  #:prop (or/c (hash/c string? any/c) #f)
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
  [composite-transition (->transition [(between/c 0 1)
                                       (between/c 0 1)
                                       (between/c 0 1)
                                       (between/c 0 1)]
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
                       filter?)]

  [producer-length (-> producer? (or/c nonnegative-integer? #f))]
  [producer-start (-> producer? (or/c nonnegative-integer? #f))]
  [producer-end (-> producer? (or/c nonnegative-integer? #f))])

  external-video)

(define (blank length)
  (if length
      (make-blank #:length length
                  #:prop-default-proc (λ (prop key [extra-data #f])
                                        (match key
                                          ["start" (mlt-prop-default-proc prop "in" extra-data)]
                                          ["end" (- (mlt-prop-default-proc prop "out" extra-data) 1)]
                                          [else (mlt-prop-default-proc prop key extra-data)]))
                  #:prop (hash "start" 0
                              "end" length))
      (color "white")))

(define-producer (clip path)
  #:source clip-path
  (define clip-path (path->string (path->complete-path path))))

(define-producer (image path)
  #:source (format "pixbuf:~a" image-path)
  #:unbounded? #t
  (define image-path (path->string (path->complete-path path))))

(define-producer (color c)
  #:source (format "color:0x~a~a~a~a"
                   (number->2string (send c* red))
                   (number->2string (send c* green))
                   (number->2string (send c* blue))
                   (number->2string (inexact->exact (round (* 255 (send c* alpha))))))
  #:unbounded? #t
  (define c*
    (match c
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% c)])))

(define (multitrack #:transitions [transitions '()]
                    #:start [maybe-start #f]
                    #:end [maybe-end #f]
                    #:length [maybe-length #f]
                    #:prop [prop #f]
                    #:filters [maybe-filters #f]
                    . tracks)
  (define start (or maybe-start (and maybe-length 0)))
  (define end (or maybe-end maybe-length))
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
                   #:start start
                   #:end end
                   #:prop (or prop (hash))
                   #:prop-default-proc (λ (prop key [default #f])
                                         (match key
                                           ["start" (mlt-prop-default-proc prop "in" default)]
                                           ["end" (add1 (mlt-prop-default-proc prop "out" default))]
                                           [_ (mlt-prop-default-proc prop key default)]))
                   #:filters (or maybe-filters '())))

(define (playlist #:transitions [transitions '()]
                  #:start [maybe-start #f]
                  #:end [maybe-end #f]
                  #:length [maybe-length #f]
                  #:prop [prop #f]
                  #:filters [maybe-filters #f]
                  . clips)
  (define start (or maybe-start (and maybe-length 0)))
  (define end (or maybe-end maybe-length))
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
   #:start start
   #:end end
   #:prop (or prop (hash))
   #:prop-default-proc (λ (prop key [default #f])
                         (match key
                           ["start" (mlt-prop-default-proc prop "in" default)]
                           ["end" (add1 (mlt-prop-default-proc prop "out" default))]
                           [_ (mlt-prop-default-proc prop key default)]))
   #:filters (or maybe-filters '())))

(define (attach-filter obj . f)
  (define new-filters (append f (service-filters obj)))
  (copy-video obj #:filteres new-filters))

(define (set-property obj key val)
  (define new-props (hash-set (properties-prop obj) key val))
  (copy-video obj #:prop new-props))

(define-transition (fade-transition)
  #:direction s/e
  #:type 'luma)

(define-transition (composite-transition x y w h)
  #:direction t/b
  #:type 'composite
  #:source (format "~a%/~a%:~a%x~a%"
                   (inexact->exact (round (* x 100)))
                   (inexact->exact (round (* y 100)))
                   (inexact->exact (round (* w 100)))
                   (inexact->exact (round (* h 100)))))

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
              #:start (or start (producer-start producer))
              #:end (or end (producer-end producer))))

(define (producer-length producer)
  (define s (producer-start producer))
  (define e (producer-end producer))
  (or
   (and s e (- e s))
   (and (not (unbounded-video? producer))
        (get-property producer "length" 'int))))

(define (producer-start producer)
  (or (core:producer-start producer)
      (and (not (unbounded-video? producer))
           (get-property producer "start" 'int))))

(define (producer-end producer)
  (or (core:producer-end producer)
      (and (not (unbounded-video? producer))
           (get-property producer "end" 'int))))

(define (grayscale-filter)
  (make-filter #:type 'grayscale))

(define (envelope-filter #:direction direction
                         #:length length)
  (make-filter #:type 'avformat.afade
               #:prop (hash "av.type" (symbol->string direction)
                            "av.durration" length)))

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

