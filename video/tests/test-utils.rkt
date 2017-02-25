#lang racket/base
(require (only-in video/base producer-length)
         (only-in "../private/video.rkt" producer? transition? filter?))
(require rackunit)
(require (for-syntax racket/base syntax/parse))
(provide check-producer check-producer? check-transition? check-filter?)

(define-syntax (check-producer stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-producer? p))]
    [(_ p #:len (~datum inf)) ; inf len
     #`(begin
         #,(syntax/loc stx (check-producer? p))
         #,(syntax/loc stx (check-false (producer-length p))))]
    [(_ p #:len l) ; finite len
     #`(begin
         #,(syntax/loc stx (check-producer? p))
         #,(syntax/loc stx (check-equal? (producer-length p) l)))]))

;; doesnt check length
(define-syntax (check-producer? stx)
  (syntax-parse stx ; ignore #:len
    [(_ p . rst) (syntax/loc stx (check-pred producer? p))]))

(define-syntax (check-transition? stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred transition? p))]))

(define-syntax (check-filter? stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred filter? p))]))
