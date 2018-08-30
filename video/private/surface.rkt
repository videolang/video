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
         racket/dict
         (only-in scribble/manual defproc)
         "video.rkt"
         (prefix-in internal: "video.rkt")
         (for-label "init.rkt"
                    "../core.rkt")
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/syntax
                     racket/base
                     racket/match
                     "init.rkt"))

(provide (all-defined-out))

;; Helpers ===========================================================================================

(begin-for-syntax
  (define-syntax-class doc-header
    (pattern (name:id arg-spec ...))
    (pattern (prototype:doc-header arg-spec ...))))

;; Producers =========================================================================================

(define-syntax (define-producer stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:subgraph subgraph) #:defaults ([subgraph #'#f]))
             (~optional (~seq #:properties properties-proc) #:defaults ([properties-proc #'values]))
             (~optional (~seq #:user-properties user-prop) #:defaults ([user-prop #'user-prop])))
        ...
        body ...)
     (quasisyntax/loc stx
       (define (f.name
                #:properties [user-prop (hash)]
                #:filters [filters '()]
                . f.args)
         body ...
         (make-producer
          #:subgraph subgraph
          #:filters filters
          #:prop (properties-proc user-prop))))]))

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
    [(_ prototype:doc-header
        (~optional (~seq #:return ret)) ;; TODO, don't ignore this
        content ...)
     #'(defproc ((~@ prototype)
                 [#:properties properties (hash/c string? any/c) (hash)]
                 [#:filters filters (listof filter?) '()])
         producer?
         content ...)]))

(define (producer? x)
  (internal:producer? x))

;; Filters ===========================================================================================

(define-syntax (define-filter stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:properties properties-proc) #:defaults ([properties-proc #'values]))
             (~optional (~seq #:user-properties user-prop) #:defaults ([user-prop #'user-prop]))
             (~optional (~seq #:source-props source-props-proc)
                        #:defaults ([source-props-proc
                                     #'(λ () (values #f #f))]))
             (~optional (~seq #:subgraph subgraph)
                        #:defaults ([subgraph #'(λ () #f)])))
        ...
        body ...)
     (quasisyntax/loc stx
       (define (f.name
                #:properties [user-prop (hash)]
                #:filters [filters '()]
                . f.args)
         body ...
         (make-filter #:source-props-proc source-props-proc
                      #:subgraph subgraph
                      #:filters filters
                      #:prop (properties-proc user-prop))))]))

(define-syntax (->filter stx)
  (syntax-parse stx
    [(_ [required ...]
        [optional ...]
        (~optional ret? #:defaults ([ret? #'any/c])))
     #`(->* [required ...]
            [#:properties (or/c dict? #f)
             #:filters (or/c list? #f)
             optional ...]
            (and/c filter? ret?))]))

(define-syntax (deffilter stx)
  (syntax-parse stx
    [(_ prototype:doc-header
        body ...)
     #`(defproc ((~@ prototype)
                 [#:filters filters (listof filter?) '()]
                 [#:properties properties (hash/c string? any/c) (hash)])
         filter?
         body ...)]))

(define (filter? x)
  (internal:filter? x))

;; Transitions =======================================================================================

(define-syntax (define-transition stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:properties properties-proc) #:defaults ([properties-proc #'values]))
             (~optional (~seq #:user-properties user-prop) #:defaults ([user-prop #'user-prop]))
             (~optional (~seq #:source-porps source-props-proc)
                        #:defaults ([source-props-proc
                                     #'(λ () (values #f #f #f #f))]))
             (~optional (~seq #:track1-subgraph track1-subgraph-proc)
                        #:defaults ([track1-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:track2-subgraph track2-subgraph-proc)
                        #:defaults ([track2-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:combined-subgraph combined-subgraph-proc)
                        #:defaults ([combined-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:prod-1 p1) #:defaults ([p1 #'p1]))
             (~optional (~seq #:prod-2 p2) #:defaults ([p2 #'p2])))
        ...
        body ...)
     (quasisyntax/loc stx
       (define (f.name #:start [p2 #f] #:end [p1 #f]
                       #:properties [user-prop (hash)]
                       . f.args)
         body ...
         (define trans
           (make-transition
            #:source-props-proc source-props-proc
            #:track1-subgraph track1-subgraph-proc
            #:track2-subgraph track2-subgraph-proc
            #:combined-subgraph combined-subgraph-proc
            #:prop (properties-proc user-prop)))
         (if (and p1 p2)
             (make-field-element #:element trans #:track p1 #:track-2 p2)
             trans)))]))

(define-syntax (->transition stx)
  (syntax-parse stx
    [(_ [required ...]
        [optional ...]
        (~optional ret? #:defaults ([ret? #'any/c])))
     #`(->* [required ...]
            [#:properties (or/c dict? #f)
             #:start (or/c any/c #f)
             #:end (or/c any/c #f)
             optional ...]
            (and/c (or/c transition? field-element?) ret?))]))

(define-syntax (deftransition stx)
  (syntax-parse stx
    [(_ prototype:doc-header
        body ...)
     #`(defproc ((~@ prototype)
                 [#:start start (or/c any/c #f) #f]
                 [#:end end (or/c any/c #f) #f]
                 [#:properties properties (hash/c string? any/c) (hash)])
         (or/c transition? field-element?)
         body ...)]))

(define (transition? x)
  (internal:transition? x))

;; Merge =============================================================================================

(define-syntax (define-merge stx)
  (syntax-parse stx
    [(_ f:function-header
        (~or (~optional (~seq #:properties properties-proc) #:defaults ([properties-proc #'values]))
             (~optional (~seq #:user-properties user-prop) #:defaults ([user-prop #'user-prop]))
             (~optional (~seq #:source-props source-props-proc)
                        #:defaults ([source-props-proc
                                     #'(λ () (values #f #f #f #f))]))
             (~optional (~seq #:track1-subgraph track1-subgraph-proc)
                        #:defaults ([track1-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:track2-subgraph track2-subgraph-proc)
                        #:defaults ([track2-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:combined-subgraph combined-subgraph-proc)
                        #:defaults ([combined-subgraph-proc #'(λ () #f)]))
             (~optional (~seq #:prod-1 p1) #:defaults ([p1 #'p1]))
             (~optional (~seq #:prod-2 p2) #:defaults ([p2 #'p2])))
        ...
        body ...)
     (quasisyntax/loc stx
       (define (f.name #:top [p2 #f] #:bottom [p1 #f]
                       #:properties [user-prop (hash)]
                       . f.args)
         body ...
         (define trans
           (make-transition
            #:source-props-proc source-props-proc
            #:track1-subgraph track1-subgraph-proc
            #:track2-subgraph track2-subgraph-proc
            #:combined-subgraph combined-subgraph-proc
            #:prop (properties-proc user-prop)))
         (if (and p1 p2)
             (make-field-element #:element trans #:track p1 #:track-2 p2)
             trans)))]))

(define-syntax (->merge stx)
  (syntax-parse stx
    [(_ [required ...]
        [optional ...]
        (~optional ret? #:defaults ([ret? #'any/c])))
     #`(->* [required ...]
            [#:properties (or/c dict? #f)
             #:top (or/c any/c #f)
             #:bottom (or/c any/c #f)
             optional ...]
            (and/c (or/c transition? field-element?) ret?))]))

(define-syntax (defmerge stx)
  (syntax-parse stx
    [(_ prototype:doc-header
        body ...)
     #`(defproc ((~@ prototype)
                 [#:top top (or/c any/c #f) #f]
                 [#:bottom bottom (or/c any/c #f) #f]
                 [#:properties properties (hash/c string? any/c) (hash)])
         (or/c merge? field-element?)
         body ...)]))

(define (merge? x)
  (internal:transition? x))
