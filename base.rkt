#lang racket/base

(require racket/contract/base
         racket/class
         racket/draw
         racket/match
         "private/video.rkt")

(provide
 (contract-out
  ;; Creates a producer that plays a clip from a file
  [clip (->* [(or/c path-string? path?)]
             [#:in (or/c nonnegative-integer? #f)
              #:out (or/c nonnegative-integer? #f)]
             producer?)]

  ;; Creates a producer that is a solid color
  [color (->* [(or/c string? (is-a?/c color%)
                     (list/c byte? byte? byte?))]
              [#:in (or/c nonnegative-integer? #f)
               #:out (or/c nonnegative-integer? #f)]
              producer?)]))

(define nonnegative-integer?
  (or/c (and/c (>=/c 0) integer?)))

(define (clip path
              #:in [in #f]
              #:out [out #f])
  (define clip-path (path->string (path->complete-path path)))
  ;; XXX: Add in/out props
  (make-producer #:source clip-path))

(define (color c
               #:in [in #f]
               #:out [out #f])
  (define c*
    (match c
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% c)]))
  ;; XXX: Add in/out props
  (make-producer #:type 'color
                 #:source
                 (format "0x~x~x~x~x"
                         (send c* red)
                         (send c* green)
                         (send c* blue)
                         (inexact->exact (round (* 255 (send c* alpha)))))))
