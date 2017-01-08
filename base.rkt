#lang racket/base

(require racket/contract/base
         racket/class
         racket/draw
         racket/match
         racket/format
         (except-in pict frame blank)
         "private/video.rkt")

(provide
 (contract-out
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (-> any/c ... producer?)]
  
  ;; Creates a blank video, for offsetting
  ;;  clips in a playlist
  [blank (-> nonnegative-integer? blank?)]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->* [(or/c path-string? path?)]
             [#:in (or/c nonnegative-integer? #f)
              #:out (or/c nonnegative-integer? #f)]
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
  [picture (->* [pict?]
                [#:length (or/c nonnegative-integer? #f)]
                producer?)]))

(define (blank length)
  (make-blank #:length length))

(define (clip path
              #:in [in #f]
              #:out [out #f])
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

(define (multitrack . tracks)
  (make-multitrack #:tracks tracks))

(define (picture p #:length [length #f])
  (error "TODO"))

(define (attach-filter obj . f)
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

;; Contract to see if a number is a nonnegative integer
;; (-> any/c boolean?)
;; Given: 0 Expect: #t
;; Given: 5 Expect: #t
;; Given: -1 Expect: #f
;; Given: 2.5 Expect: #f
(define nonnegative-integer?
  (or/c (and/c (>=/c 0) integer?)))
