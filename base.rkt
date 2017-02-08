#lang racket/base

(require racket/contract/base
         racket/class
         racket/draw
         racket/match
         racket/format
         racket/list
         (except-in pict frame blank)
         "private/video.rkt"
         (prefix-in core: "private/video.rkt"))

(provide
 (contract-out
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (->* []
                   [#:transitions (listof transition?)]
                   #:rest (listof any/c)
                   producer?)]

  ;; Creates a playlist (tracks playing in sequence)
  ;;   (syntactic sugar for list)
  [playlist (->* []
                 [#:transitions (listof transition?)]
                 #:rest (listof any/c)
                 producer?)]
  
  ;; Creates a blank video, for offsetting
  ;;  clips in a playlist
  [blank (-> nonnegative-integer? blank?)]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->* [(or/c path-string? path?)]
             [#:in (or/c nonnegative-integer? #f)
              #:out (or/c nonnegative-integer? #f)
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

  ;; Creates a clip who's producer is a pict
  [image (->* [pict?]
                [#:length (or/c nonnegative-integer? #f)]
                producer?)]

  [fade-transition (->* [#:length nonnegative-integer? #f]
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
                          #:length integer?]
                         [#:top (or/c any/c #f)
                          #:bottom (or/c any/c #f)]
                         transition?)]
  
  [scale-filter (case-> (-> (and/c number? positive?) (and/c number? positive?) filter?)
                        (-> service? (and/c number? positive?) (and/c number? positive?) service?))]))

(define (blank length)
  (make-blank #:length length))

(define (clip path
              #:in [in #f]
              #:out [out* #f]
              #:length [other-out #f])
  (define out (or out* other-out))
  (define clip-path (path->string (path->complete-path path)))
  (define prop*
    (let* ([prop (hash)]
           [prop (if in (hash-set prop "in" in) prop)]
           [prop (if out (hash-set prop "out" out) prop)])
      prop))
  (make-producer #:source clip-path
                 #:prop prop*))

(define (color c #:length [length #f])
  (define c*
    (match c
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% c)]))
  (define prop*
    (let* ([prop (hash)]
           [prop (if length (hash-set prop "out" length) prop)])
      prop))
  (make-producer #:type 'color
                 #:source (format "0x~a~a~a~a"
                                  (number->2string (send c* red))
                                  (number->2string (send c* green))
                                  (number->2string (send c* blue))
                                  (number->2string (inexact->exact (round (* 255 (send c* alpha))))))
                 #:prop prop*))

(define (multitrack #:transitions [transitions '()] . tracks)
  (make-multitrack #:tracks tracks
                   #:transitions (map transition->field-element transitions)))

(define (playlist #:transitions [transitions '()] . clips)
  (for/fold ([acc clips])
            ([t (in-list transitions)])
    (define start (transition-start transition))
    (define end (transition-end transition))
    (append*
     (for/list ([clip (in-list clips)])
       (cond
        [(equal? start clip)
         (list clip (transition-transition t))]
        [(and (not start) (equal? end clip))
         (list (transition-transition t) clip)]
        [else (list clip)])))))

(define (image path #:length [length #f])
  (define image-path (path->string (path->complete-path path)))
  (define prop*
    (let* ([prop (hash)]
           [prop (if length (hash-set prop "in" length) prop)]
           [prop (if length (hash-set prop "out" length) prop)])
      prop))
  (make-producer #:source (format "pixbuf:~a" image-path)
                 #:prop prop*))

(define (attach-filter obj . f)
  (error "TODO"))

(define (fade-transition #:length length
                         #:start [start #f]
                         #:end [end #f])
  (transition (make-transition #:type 'luma
                               #:length length)
              start
              end))

(define (composite-transition x y w h
                              #:top [top #f]
                              #:bottom [bottom #f])
  (transition (make-transition #:type 'composite
                               #:source (format "~a%/~a%:~a%x~a%"
                                                (inexact->exact (round (* x 100)))
                                                (inexact->exact (round (* y 100)))
                                                (inexact->exact (round (* w 100)))
                                                (inexact->exact (round (* h 100)))))
              bottom
              top))

(define (swipe-transition x y w h
                          #:top [top #f]
                          #:bottom [bottom #f])
  (error "TODO"))

(define scale-filter
  (case-lambda
    [(p w h) (error "TODO")]
    [(w h) (error "TODO")]))

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

;; Contract to see if a number is a nonnegative integer
;; (-> any/c boolean?)
;; Given: 0 Expect: #t
;; Given: 5 Expect: #t
;; Given: -1 Expect: #f
;; Given: 2.5 Expect: #f
(define nonnegative-integer?
  (or/c (and/c (>=/c 0) integer?)))

(struct transition (transition
                    start
                    end))

;; Converts a transition into a field element (for multitracks)
;; (-> transition? field-element?)
(define (transition->field-element transition)
  (make-field-element #:transition (transition-transition transition)
                      #:track (transition-start transition)
                      #:track-2 (transition-end transition)))
