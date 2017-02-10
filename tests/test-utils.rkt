#lang racket/base
(require video/lib
         (only-in video/core producer-end producer-start properties-prop)
         (only-in "../private/video.rkt" producer?))
(require rackunit)
(require (for-syntax racket/base syntax/parse))
(provide check-producer check-producer?)

(define (producer-len p)
  (define props (properties-prop p))
  (define start (hash-ref props "in" 0))
  (define end (hash-ref props "out" #f))
  (and end (- end start)))

(define-syntax (check-producer stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred producer? p))]
    [(_ p #:len l) ; inf len = #f
     #`(begin
         #,(syntax/loc stx (check-producer p))
         #,(syntax/loc stx (check-equal? (producer-len p) l)))]))

(define-syntax-rule (check-producer? p) (check-producer p))
