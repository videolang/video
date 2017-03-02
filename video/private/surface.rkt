#lang racket/base

;; This library will probably become (at some point) public
;; so users can define their own producers. But for now its private due to
;; how much it will probably change.

(require "video.rkt"
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base))

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
         (define end* (or end (and len 0)))
         (define properties
           (hash-set* prop "start" start* "end" end* "length" (and start* end* (- end* start* 0))))
         body ...
         (make-producer #:type type
                        #:source source
                        #:start prod-start
                        #:end prod-end
                        #:prop properties))]))
