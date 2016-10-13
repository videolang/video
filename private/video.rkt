#lang racket

(provide (all-defined-out))
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(define-syntax subclass-empty '(() . ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super*:id [ids:id default] ...)
     #:with constructor (format-id stx "make-~a" #'name)
     #:with new-supers (format-id stx "subclass-~a" #'name)
     #:with super (format-id stx "subclass-~a" #'super*)
     (define super-vals (syntax-local-value #'super))
     (define all-ids (append (car super-vals) (syntax->datum #'(ids ...))))
     (define all-defaults (append (cdr super-vals) (syntax-e #'(default ...))))
     #`(begin
         (define (constructor #,@(append*
                                  (for/list ([i (in-list all-ids)]
                                             [j (in-list all-defaults)])
                                    `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                      [,i ,j]))))
           (name #,@all-ids))
         (define-syntax new-supers '#,(cons all-ids all-defaults)))]))

(struct video ([mlt-object #:mutable]))
(define-constructor video empty [mlt-object #f])
(struct link video (source target index))
(define-constructor link video [source #f] [target #f] [index 0])
(struct properties video (prop))
(define-constructor properties video [prop (make-hash)])
(struct frame properties ())
(define-constructor frame properties)
(struct service properties (filters))
(define-constructor service properties [filters '()])
(struct filter service (type))
(define-constructor filter service [type #f])
(struct transition service (type playlist index length))
(define-constructor transition service [type #f] [playlist '()] [index #f] [length #f])
(struct consumer service (type target))
(define-constructor consumer service [type #f] [target #f])
(struct producer service (type source start end))
(define-constructor producer service [type #f] [source #f] [start #f] [end #f])
(struct playlist producer (producers))
(define-constructor playlist producer [producers '()])
(struct tractor producer (inputs output))
(define-constructor tractor producer [inputs '()] [output #f])
(struct multitrack producer (tracks))
(define-constructor multitrack producer [tracks '()])
(struct field producer (filters/transitions))
(define-constructor field producer [filters/transitions '()])
