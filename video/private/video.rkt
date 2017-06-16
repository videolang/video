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

(provide (all-defined-out))
(require racket/dict
         racket/match
         racket/set
         racket/class
         racket/list
         racket/generic
         file/convertible
         (prefix-in file: file/convertible)
         "utils.rkt"
         "ffmpeg.rkt"
         "ffmpeg-pipeline.rkt"
         "init.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(define current-renderer (make-parameter #f))
(define current-profile (make-parameter #f))
(define current-video-directory (make-parameter (current-directory)))
(define current-skip-memoize? (make-parameter #f))
(define current-video-start (make-parameter #f))
(define current-video-end (make-parameter #f))

;; A helper function to convert videos to MLT object
;; Video (U Renderer% #f) -> _mlt-object
(define (convert source
                 #:renderer [renderer* #f])
  (define renderer (or renderer* (current-renderer)))
  (unless renderer
    (error 'current-renderer "No renderer set"))
  (send renderer prepare source))

;; Helper function to determine if a producer
;; can be potentially unbounded in length.
;; Producer -> Boolean
(define (unbounded-video? prod)
  (cond
    [(playlist? prod) (ormap unbounded-video? (playlist-elements prod))]
    [(multitrack? prod) (andmap unbounded-video? (multitrack-tracks prod))]
    [(producer? prod) (producer-unbounded? prod)]
    [else #f])) ;; Should only happen in playlists

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; Properties Path -> Void
(define (debug/save-prop prop filepath)
  (error "TODO"))

(define (finish-video-object-init! video-object video)
  ;; Set properties
  (when (properties? video)
    (for ([(k* v*) (in-dict (properties-prop video))])
      (define-values (k v) ((properties-mlt-default-proc video) k* v*))
      (error "TODO")))
  ;; Attach filters
  (when (service? video)
    (for ([f (in-list (service-filters video))])
      (error "TODO"))))

;; Dynamic Dispatch for Video Objects
(define-generics video-ops
  (copy-video-op video-ops to-copy))
(define copy-video
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (unless (= 1 (length args))
       (error 'copy-video "copy-video requires exactly one non keyword argument"))
     (copy-video-op (first args)
                    (map cons
                         (map (compose string->symbol keyword->string) kws)
                         kw-args)))))

;; Constructor for video objects
(define-syntax subclass-empty '(() () ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super* ([ids:id default] ...) body ...)
     #:with constructor (format-id stx "make-~a" #'name)
     #:with this (format-id stx "this")
     #:with new-supers (format-id stx "subclass-~a" #'name)
     #:with super (format-id stx "subclass-~a" (if (identifier? #'super*)
                                                   #'super*
                                                   #'empty))
     #:with predicate (format-id stx "~a?" #'name)
     #:with convert-name (format-id stx "convert-~a" #'name)
     (define super-vals (syntax-local-value #'super))
     (define all-structs (append (first super-vals) (make-list (length (syntax->list #'(ids ...)))
                                                               (syntax->datum #'name))))
     (define all-ids (append (second super-vals) (syntax->datum #'(ids ...))))
     (define all-defaults (append (third super-vals) (syntax-e #'(default ...))))
     (quasisyntax/loc stx
       (begin
         (struct name #,@(if (identifier? #'super*) (list #'super*) '())
           (ids ...)
           #:transparent
           #:methods gen:video-ops
           [(define (copy-video-op v to-copy)
              (name
               #,@(for/list ([i (in-list all-structs)]
                             [j (in-list all-ids)])
                    #`(if (dict-has-key? to-copy '#,j)
                          (dict-ref to-copy '#,j)
                          (#,(format-id stx "~a-~a" i j) v)))))]
           #:property prop:convertible
           (let ([memo-table (make-hasheq)])
             (λ (v request def)
               (match request
                 ['mlt
                  (define (conv)
                    (define ret
                      (let ([this v])
                        (let #,(for/list ([i (in-list all-structs)]
                                          [j (in-list all-ids)])
                                 #`[#,(datum->syntax stx j)
                                    (#,(format-id stx "~a-~a" i j) v)])
                          #f body ...)))
                    (when ret
                      (finish-video-object-init! ret v))
                    ret)
                  (if (current-skip-memoize?)
                      (conv)
                      (hash-ref! memo-table v conv))]
                 [_ def]))))
         (define (constructor #,@(append*
                                  (for/list ([i (in-list all-ids)]
                                             [j (in-list all-defaults)])
                                    `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                      [,(datum->syntax stx i) ,j]))))
           (name #,@(map (curry datum->syntax stx) all-ids)))
         (define-syntax new-supers '#,(list all-structs all-ids all-defaults))))]))

;; Structs
(define-constructor video #f ())

(define-constructor link video ([source #f] [target #f] [index 0])
  (error "TODO"))

(define-constructor properties video ([prop (hash)]
                                      [prop-default-proc mlt-prop-default-proc]
                                      [mlt-default-proc mlt-mlt-default-proc])
  (error "TODO"))

(define (get-property dict key
                      [extra-info #f])
  (dict-ref (properties-prop dict) key
            (λ () ((properties-prop-default-proc dict) dict key extra-info))))

(define (mlt-prop-default-proc dict key default-type)
  (define v (convert dict))
  (unless v
    (error 'properties "MLT object for ~a not created, cannot get default property" dict))
  (error "TODO"))

(define (mlt-mlt-default-proc key val)
  (match key
    ["start" (values "in" val)]
    ["end" (values "out" (and val (- val 1)))]
    [else (values key val)]))

(define-constructor anim-property video ([value #f] [position #f] [length #f]))

(define-constructor frame properties ())

(define-constructor service properties ([filters '()]))

(define-constructor filter service ([type #f] [source #f])
  (error "TODO"))

(define-constructor transition service ([type #f] [source #f] [length #f])
  (error "TODO"))

(define-constructor consumer service ([type #f] [target #f])
  (error "TODO"))

(define-constructor producer service ([type #f]
                                      [source #f]
                                      [start #f]
                                      [end #f]
                                      [speed #f]
                                      [seek #f]
                                      [unbounded? #f])
  (error "TODO"))

(define-constructor file producer ([file #f])
  (error "TODO"))

(define-constructor blank producer ([length 0])
  (convert (make-playlist #:elements (list this))))

(define-constructor playlist producer ([elements '()])
  (error "TODO"))

(define-constructor multitrack producer ([tracks '()] [field '()])
  (error "TODO"))

(define-constructor field-element video ([element #f] [track #f] [track-2 #f]))

;; The render module sets a parameter we rely on
;;  (Yes, we 'could' do it with units, but requires a large
;;   amount of boilerplate.)
(void (dynamic-require (build-path video-dir "render.rkt") #f))
