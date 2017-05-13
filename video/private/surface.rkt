#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(require racket/match
         racket/contract/base
         racket/math
         (only-in scribble/manual defproc)
         "video.rkt"
         (for-label "init-mlt.rkt"
                    "../core.rkt")
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base
                     racket/match
                     "init-mlt.rkt"))

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
                #:filters [filters '()]
                . f.args)
         (when (and len (or start end))
           (error "Cannot define both start/end and length"))
         (when (and start end (> start end))
           (error "Start of clip cannot be larger than its end"))
         (define start* (or start (and len 0) 0))
         (define end* (or end len -1))
         (define length* (or len (and start end (- end start)) -1))
         (define properties
           #,(if (syntax-e (attribute unbounded?))
                 #'(hash-set* prop
                              "start" start*
                              "end" end*)
                 #'prop))
         body ...
         (make-producer
          #:type type
          #:source source
          #:start prod-start
          #:end prod-end
          #:unbounded? (and unbounded? (not (or prod-start prod-end)))
          #:filters filters
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
             #:filters (list/c filter?)
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
                    [#:properties properties (hash/c string? any/c) (hash)]
                    [#:filters filters (listof filter?) '()])
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
                            [(or 's/e 'start/end _) #'(#:start arg1 #:end arg2)])
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
                  [(or 's/e 'start/end _) #'(#:start (or/c any/c #f) #:end (or/c any/c #f))])
             optional ...]
            (and/c (or/c transition? field-element?) ret?))]))

(define-syntax (deftransition stx)
  (syntax-parse stx
    [(_ (id:id args ...)
        (~optional (~seq #:direction direction))
        body ...)
     #`(defproc (id args ...
                    [#:length length (or/c nonnegative-integer? #f) #f]
                    #,@(match (syntax-e (attribute direction))
                         [(or 't/b 'top/bottom) #'([#:top top (or/c any/c #f) #f]
                                                   [#:bottom bottom (or/c any/c #f) #f])]
                         [(or 's/e 'start/end _) #'([#:start start (or/c any/c #f) #f]
                                                    [#:end end (or/c any/c #f) #f])]))
         (or/c transition? field-element?)
         body ...)]))
