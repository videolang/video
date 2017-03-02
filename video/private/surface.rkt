#lang racket/base

;; This library will probably become (at some point) public
;; so users can define their own producers. But for now its private due to
;; how much it will probably change.

(require "video.rkt"
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base
                     racket/match))

(provide (all-defined-out))

(define-syntax (define-producer stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:drop (datums ...)) #:defaults ([(datums 1) #'()]))
             (~optional (~seq #:type type) #:defaults ([type #'#f]))
             (~optional (~seq #:source source) #:defaults ([source #'#f]))
             (~optional (~seq #:start prod-start) #:defaults ([prod-start #'start*]))
             (~optional (~seq #:end prod-end) #:defaults ([prod-end #'end*]))
             (~optional (~seq #:length length*) #:defaults ([length* #'length*]))
             (~optional (~seq #:properties prop*) #:defaults ([prop* #'properties])))
        ...
        body ...)
     #'(define (f.name
                       #:start [start #f]
                       #:end [end #f]
                       #:length [len #f]
                       #:properties [prop (hash)]
                       . f.args)
         (when (and len (or start end))
           (error "Cannot define both start/end and length"))
         (define start* (or start (and len 0)))
         (define end* (or end len))
         (define properties
           (hash-set* prop "start" start* "end" end* "length" (and start* end* (- end* start* 0))))
         body ...
         (make-producer #:type type
                        #:source source
                        #:start prod-start
                        #:end prod-end
                        #:prop properties))]))

(define-syntax (define-transition stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:type type) #:defaults ([type #'#f]))
             (~optional (~seq #:source source) #:defaults ([source #'#f]))
             (~optional (~seq #:length trans-len) #:defaults ([trans-len #'len*]))
             (~optional (~seq #:direction direction))
             (~optional (~seq #:prod-1 p1) #:defaults ([p1 #'p1]))
             (~optional (~seq #:prod-2 p2) #:defaults ([p2 #'p2])))
        ...
        body ...)
     #:with arg1 #'[p1 #f]
     #:with arg2 #'[p2 #f]
     #`(define (f.name #,@(match (syntax-e (attribute direction))
                            [(or 't/b 'top/bottom) #'(#:top arg1 #:bottom arg2)]
                            [(or 's/e 'start/end) #'(#:start arg1 #:end arg2)]
                            [_ #'(#:top arg1 #:bottom arg2)])
                       #:length [len* #f]
                       . f.args)
         body ...
         (define trans
           (make-transition
            #:type type
            #:source source
            #:length trans-len))
         (if (and p1 p2)
             (make-field-element #:element trans #:track p1 #:track-2 p2)
             trans))]))
