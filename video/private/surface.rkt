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
         (for-label "init.rkt"
                    "../core.rkt")
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base
                     racket/match
                     "init.rkt"))

(provide (all-defined-out))

;; Producers =========================================================================================

(define-syntax (define-producer stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:type type) #:defaults ([type #'#f]))
             (~optional (~seq #:source source) #:defaults ([source #'#f]))
             (~optional (~seq #:properties properties) #:defaults ([properties #'properties])))
        ...
        body ...)
     #`(define (f.name
                #:properties [properties (hash)]
                #:filters [filters '()]
                . f.args)
         body ...
         (make-producer
          #:type type
          #:source source
          #:filters filters
          #:prop properties))]))

(define-syntax (->producer stx)
  (syntax-parse stx
    [(_ [extra-args ...]
        [optional-args ...]
        (~optional ret? #:defaults ([ret? #'any/c])))
     #'(->* [extra-args ...]
            [#:properties (hash/c string? any/c)
             #:filters (list/c filter?)
             optional-args ...]
            (and/c producer? ret?))]))

(define-syntax (defproducer stx)
  (syntax-parse stx
    [(_ (id:id args ...)
        (~optional (~seq #:return ret)) ;; TODO, don't ignore this
        content ...)
     #'(defproc (id args ...
                    [#:properties properties (hash/c string? any/c) (hash)]
                    [#:filters filters (listof filter?) '()])
         producer?
         content ...)]))

;; Transitions =======================================================================================

(define-syntax (define-transition stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:direction direction))
             (~optional (~seq #:properties properties*) #:defaults ([properties* #'properties]))
             (~optional (~seq #:track1-subgraph track1-subgraph-proc)
                        #:defaults ([track1-subgraph-proc #'(λ (x) (make-video-subgraph))]))
             (~optional (~seq #:track2-subgraph track2-subgraph-proc)
                        #:defaults ([track2-subgraph-proc #'(λ (x) (make-video-subgraph))]))
             (~optional (~seq #:combined-subgraph combined-subgraph-proc)
                        #:defaults ([combined-subgraph-proc #'(λ (x y) (make-video-subgraph))]))
             (~optional (~seq #:prod-1 p1) #:defaults ([p1 #'p1]))
             (~optional (~seq #:prod-2 p2) #:defaults ([p2 #'p2])))
        ...
        body ...)
     #:with arg1 #'[p1 #f]
     #:with arg2 #'[p2 #f]
     #`(define (f.name #,@(match (syntax-e (attribute direction))
                            [(or 't/b 'top/bottom) #'(#:top arg2 #:bottom arg1)]
                            [(or 's/e 'start/end _) #'(#:start arg2 #:end arg1)])
                       #:properties [properties #f]
                       . f.args)
         body ...
         (define trans
           (make-transition
            #:track1-subgraph track1-subgraph-proc
            #:track2-subgraph track2-subgraph-proc
            #:combined-subgraph combined-subgraph-proc
            #:properties properties*))
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
