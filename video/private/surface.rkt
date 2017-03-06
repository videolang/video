#lang racket/base

;; This library will probably become (at some point) public
;; so users can define their own producers. But for now its private due to
;; how much it will probably change.

(require racket/match
         racket/contract/base
         racket/math
         (only-in scribble/manual defproc)
         "video.rkt"
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base
                     racket/match))

(provide (all-defined-out))

;; Producers =========================================================================================

(define-syntax (define-producer stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:drop (datums ...)) #:defaults ([(datums 1) #'()]))
             (~optional (~seq #:type type) #:defaults ([type #'#f]))
             (~optional (~seq #:source source) #:defaults ([source #'#f]))
             (~optional (~seq #:start prod-start) #:defaults ([prod-start #'start*]))
             (~optional (~seq #:end prod-end) #:defaults ([prod-end #'end*]))
             (~optional (~seq #:length prod-length) #:defaults ([prod-length #'length*]))
             (~optional (~seq #:unbounded? unbounded?)
                        #:defaults ([unbounded? #'#f]))
             (~optional (~seq #:properties prop*) #:defaults ([prop* #'properties]))
             (~optional (~seq #:properties-default-porc pdp)
                        #:defaults ([pdp #'mlt-prop-default-proc])))
        ...
        body ...)
     #`(define (f.name
                #:start [start #f]
                #:end [end #f]
                #:length [len #f]
                #:properties [prop (hash)]
                . f.args)
         (when (and len (or start end))
           (error "Cannot define both start/end and length"))
         (when (and start end (> start end))
           (error "Start of clip cannot be larger than its end"))
         (define start* (or start (and len 0)))
         (define end* (or end len))
         (define length* (or len (and start end (- end start))))
         (define properties
           #,(if (syntax-e (attribute unbounded?))
                 #'(hash-set* prop
                              "start" start*
                              "end" end*
                              "length" (and start* end* (- end* start*)))
                 #'prop))
         body ...
         (make-producer
          #:type type
          #:source source
          #:start prod-start
          #:end prod-end
          #:unbounded? (and unbounded? (not (or prod-start prod-end)))
          #:prop properties
          #:prop-default-proc
          (λ (prop key [extra-data #f])
            (match key
              ["start" (or prod-start
                           #,(if (syntax-e (attribute unbounded?))
                                 #'#f
                                 #'(mlt-prop-default-proc prop "in" extra-data)))]
              ["end" (or prod-end
                         #,(if (syntax-e (attribute unbounded?))
                               #'#f
                               #'(add1 (mlt-prop-default-proc prop "out" extra-data))))]
              ["length" (or prod-length
                            #,(if (syntax-e (attribute unbounded?))
                                  #'#f
                                  #'(mlt-prop-default-proc prop "length" extra-data)))]
              [else (pdp prop key extra-data)]))))]))

(define-syntax (->producer stx)
  (syntax-parse stx
    [(_ [extra-args ...]
        [optional-args ...]
        (~optional ret? #:defaults ([ret? #'any/c])))
     #'(->* [extra-args ...]
            [#:start (or/c nonnegative-integer? #f)
             #:end (or/c nonnegative-integer? #f)
             #:length (or/c nonnegative-integer? #f)
             #:properties (hash/c string? any/c)
             optional-args ...]
            (and/c producer? ret?))]))

(define-syntax (defproducer stx)
  (syntax-parse stx
    [(_ (id:id args ...)
        (~optional (~seq #:return ret)) ;; TODO, don't ignore this
        content ...)
     #'(defproc (id args ...
                    [#:start start (or/c nonnegative-integer? #f) #f]
                    [#:end end (or/c nonnegative-integer? #f) #f]
                    [#:length length (or/c nonnegative-integer? #f) #f]
                    [#:properties properties (hash/c string? any/c) (hash)])
         producer?
         content ...)]))

;; Transitions =======================================================================================

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

(define-syntax (->transition stx)
  (syntax-parse stx
    [(_ [required ...]
        [optional ...]
        (~optional (~seq #:direction direction))
        (~optional ret? #:defaults ([ret? #'any/c])))
     #`(->* [required ...]
            [#:length (or/c nonnegative-integer? #f)
             #,@(match (syntax-e (attribute direction))
                  [(or 't/b 'top/bottom) #'(#:top (or/c any/c #f) #:bottom (or/c any/c #f))]
                  [(or 's/e 'start/end) #'(#:start (or/c any/c #f) #:end (or/c any/c #f))]
                  [_ #'(#:top (or/c any/c #f) #:bottom (or/c any/c #f))])
             optional ...]
            (and/c (or/c transition? field-element?) ret?))]))
