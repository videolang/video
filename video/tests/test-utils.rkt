#lang racket/base
(require video/base
         video/core
         "../private/utils.rkt")
(require rackunit)
(require (for-syntax racket/base
                     syntax/parse
                     "../private/utils.rkt"))
(provide check-producer check-transition check-filter)

(define-syntax (check-producer stx)
  (syntax-parse stx
    [(_ p (~or (~optional (~seq #:len len))))
     (quasisyntax/loc stx
       (begin
         #,(syntax/loc stx (check-pred producer? p))
         #,@(cond
              [(not (attribute len)) (list)]
              [(and (number? (attribute len))
                    (= (attribute len) +inf.0))
               (list (syntax/loc stx (check-false? (producer-length p))))]
              [else
               (list (syntax/loc stx (check-equal? (producer-length p) len)))])))]))

(define-syntax (check-transition stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred transition? p))]))

(define-syntax (check-filter stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred filter? p))]))
