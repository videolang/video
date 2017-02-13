#lang racket/base
(require (only-in video/base get-property)
         (only-in "../private/video.rkt"
                  producer? field-element? transition? filter?))
(require rackunit)
(require (for-syntax racket/base syntax/parse))
(provide check-producer check-producer? check-transition? check-filter?)

(define (producer-len p) (get-property p "length" 'int))

(define INF-LENGTH 1000)

(define-syntax (check-producer stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-producer? p))]
    [(_ p #:len (~datum inf)) ; inf len
     #`(begin
         #,(syntax/loc stx (check-producer? p))
         #,(syntax/loc stx (check-true (> (producer-len p) INF-LENGTH))))]
    [(_ p #:len l) ; inf len
     #`(begin
         #,(syntax/loc stx (check-producer? p))
         #,(syntax/loc stx (check-equal? (producer-len p) l)))]))

;; doesnt check length
(define-syntax (check-producer? stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred producer? p))]))

(define-syntax (check-transition? stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred transition? p))]))

(define-syntax (check-filter? stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred filter? p))]))
