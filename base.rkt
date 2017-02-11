#lang racket/base

(require racket/contract/base
         racket/class
         racket/draw
         racket/match
         racket/format
         racket/list
         (except-in pict frame blank)
         "private/video.rkt"
         "private/utils.rkt"
         (prefix-in core: "private/video.rkt"))

(provide
 (contract-out
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (->* []
                   [#:transitions (listof transition?)
                    #:start (or/c nonnegative-integer? #f)
                    #:end (or/c nonnegative-integer? #f)
                    #:length (or/c nonnegative-integer? #f)]
                   #:rest (listof any/c)
                   producer?)]

  ;; Creates a playlist (tracks playing in sequence)
  ;;   (syntactic sugar for list)
  [playlist (->* []
                 [#:transitions (listof transition?)
                  #:start (or/c nonnegative-integer? #f)
                  #:end (or/c nonnegative-integer? #f)
                  #:length (or/c nonnegative-integer? #f)]
                 #:rest (listof any/c)
                 producer?)]

  ;; Creates a blank video, for offsetting
  ;;  clips in a playlist
  [blank (-> nonnegative-integer? blank?)]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->* [(or/c path-string? path?)]
             [#:start (or/c nonnegative-integer? #f)
              #:end (or/c nonnegative-integer? #f)
              #:length (or/c nonnegative-integer? #f)]
             producer?)]

  ;; Creates a producer that is a solid color
  [color (->* [(or/c string? (is-a?/c color%)
                     (list/c byte? byte? byte?))]
              [#:length (or/c nonnegative-integer? #f)]
              producer?)]

  ;; Create a producer that is the same as the other producer but with one or more
  ;; filters attached to it
  [attach-filter (-> service? filter? ... producer?)]

  ;; Creates a clip who's producer is path
  [image (->* [(or/c path-string? path-for-some-system?)]
              [#:length (or/c nonnegative-integer? #f)]
              producer?)]

  [fade-transition (->* [#:length nonnegative-integer?]
                        [#:start any/c
                         #:end any/c]
                        transition?)]

  ;; Creates a composite transition where the top track is
  ;;   placed above the bottom track
  [composite-transition (->* [(between/c 0 1)
                              (between/c 0 1)
                              (between/c 0 1)
                              (between/c 0 1)]
                             [#:top (or/c any/c #f)
                              #:bottom (or/c any/c #f)]
                             transition?)]

  [swipe-transition (->* [#:direction symbol?
                          #:length nonnegative-integer?]
                         [#:top (or/c any/c #f)
                          #:bottom (or/c any/c #f)]
                         transition?)]
  
  [scale-filter (case-> (-> (and/c number? positive?) (and/c number? positive?) filter?)
                        (-> service? (and/c number? positive?) (and/c number? positive?) service?))]

  [grayscale-filter (case-> (-> filter?)
                            (-> service? service?))]

  [get-property (->* [properties? string?]
                     [symbol?]
                     any/c)]))

(define (blank length)
  (make-blank #:length length))

(define (clip path
              #:start [in* #f]
              #:end [out* #f]
              #:length [other-out #f])
  (define in (or in* (and other-out 0)))
  (define out (or out* other-out))
  (define clip-path (path->string (path->complete-path path)))
  (define prop*
    (let* ([prop (hash)]
           [prop (if in (hash-set prop "in" in) prop)]
           [prop (if out (hash-set prop "out" out) prop)])
      prop))
  (make-producer #:source clip-path
                 #:start (and in 0)
                 #:end out))

(define (color c #:length [length #f])
  (define c*
    (match c
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% c)]))
  (make-producer #:type 'color
                 #:source (format "0x~a~a~a~a"
                                  (number->2string (send c* red))
                                  (number->2string (send c* green))
                                  (number->2string (send c* blue))
                                  (number->2string (inexact->exact (round (* 255 (send c* alpha))))))
                 #:start (and length 0)
                 #:end length))

(define (multitrack #:transitions [transitions '()]
                    #:start [maybe-start #f]
                    #:end [maybe-end #f]
                    #:length [maybe-length #f]
                    . tracks)
  (define start (or maybe-start (and maybe-length 0)))
  (define end (or maybe-end maybe-length))
  (define-values (tracks* transitions*)
    (for/fold ([tracks* '()]
               [transitions* transitions])
              ([track (in-list tracks)]
               [i (in-naturals)])
      (cond
        [(transition? track) (values tracks*
                                     (cons (make-field-element #:element track
                                                               #:track (list-ref tracks (sub1 i))
                                                               #:track-2 (list-ref tracks (add1 i)))
                                           transitions*))]
        [else (values (cons track tracks*) transitions*)])))
  (make-multitrack #:tracks (reverse tracks*)
                   #:field transitions*
                   #:start start
                   #:end end))

(define (playlist #:transitions [transitions '()]
                  #:start [maybe-start #f]
                  #:end [maybe-end #f]
                  #:length [maybe-length #f]
                  . clips)
  (define start (or maybe-start (and maybe-length 0)))
  (define end (or maybe-end maybe-length))
  (make-playlist
   #:elements
   (for/fold ([acc clips])
             ([t (in-list transitions)])
     (define start (field-element-track transition))
     (define end (field-element-track-2 transition))
     (append*
      (for/list ([clip (in-list clips)])
        (cond
          [(equal? start clip)
           (list clip (field-element-element t))]
          [(and (not start) (equal? end clip))
           (list (field-element-element t) clip)]
          [else (list clip)]))))
   #:start start
   #:end end))

(define (image path #:length [length #f])
  (define image-path (path->string (path->complete-path path)))
  (make-producer #:source (format "pixbuf:~a" image-path)
                 #:start (and length 0)
                 #:end length))

;; TODO, sigh, we should really not have a closed
;;    world assumption here. :'(
(define (attach-filter obj . f)
  (define new-filters (append f (service-filters obj)))
  (cond
    [(filter? obj)
     (struct-copy filter obj [filters #:parent service new-filters])]
    [(core:transition? obj)
     (struct-copy core:transition obj [filters #:parent service new-filters])]
    [(playlist? obj)
     (struct-copy core:playlist obj [filters #:parent service new-filters])]
    [(multitrack? obj)
     (struct-copy core:multitrack obj [filters #:parent service new-filters])]
    [(consumer? obj)
     (struct-copy consumer obj [filters #:parent service new-filters])]
    [(blank? obj)
     (struct-copy consumer obj [filters #:parent service new-filters])]
    [(producer? obj)
     (struct-copy producer obj [filters #:parent service new-filters])]
    [else
     (struct-copy service obj [filters new-filters])]))

(define (fade-transition #:length length
                         #:start [start #f]
                         #:end [end #f])
  (define trans
    (make-transition #:type 'luma
                     #:length length))
  (if (and start end)
      (make-field-element #:element trans #:track start #:track-2 end)
      trans))

(define (composite-transition x y w h
                              #:top [top #f]
                              #:bottom [bottom #f])
 (define trans
   (make-transition #:type 'composite
                    #:source (format "~a%/~a%:~a%x~a%"
                                     (inexact->exact (round (* x 100)))
                                     (inexact->exact (round (* y 100)))
                                     (inexact->exact (round (* w 100)))
                                     (inexact->exact (round (* h 100))))))
  (if (and top bottom)
      (make-field-element #:element trans
                          #:track bottom
                          #:track-2 top)
      trans))

(define (swipe-transition #:direction dir
                          #:length length
                          #:top [top #f]
                          #:bottom [bottom #f])
  (error "TODO"))

(define scale-filter
  (case-lambda
    [(p w h) (attach-filter p (scale-filter w h))]
    [(w h) (make-filter #:type 'frei0r.scale0tilt
                        #:prop (hash "4" w "5" h))]))

(define grayscale-filter
  (case-lambda
    [(p) (attach-filter p (grayscale-filter))]
    [() (make-filter #:type 'grayscale)]))

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

