#lang racket/base

(provide (all-defined-out))
(require racket/dict
         racket/match
         racket/set
         "mlt.rkt"
         "init-mlt.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(define profile (mlt-profile-init #f))

;; Calls mlt-*-service on the correct data type
;;    (getting the service type)
;; Service -> _mlt-service
(define (mlt-*-service video-object)
  (cond
    [(link? video-object)
     (mlt-*-service (link-target video-object))]
    [(filter? video-object)
     (mlt-filter-service (video-mlt-object video-object))]
    [(producer? video-object)
     (mlt-producer-service (video-mlt-object video-object))]
    [else (error 'video "Unsupported video: ~a" video-object)]))

;; Connect target to source
;; Video-Object _mlt-service Integer -> Void
(define (mlt-*-connect target source-service [index #f])
  (cond
    [(consumer? target)
     (mlt-consumer-connect (video-mlt-object target)
                           source-service)]
    [(filter? target)
     (mlt-filter-connect (video-mlt-object target)
                         source-service
                         index)]
    [else (error 'video "Unsupported target ~a" target)]))


(define (finish-mlt-object-init! video)
  (define mlt-object (video-mlt-object video))
  ;; Set properties
  (when (properties? video)
    (for ([(k v) (in-dict (properties-prop video))])
      (cond
        [(integer? v) (mlt-properties-set-int64 mlt-object k v)]
        [(real? v) (mlt-properties-set-double mlt-object k v)]
        [(string? v) (mlt-properties-set mlt-object k v)]
        [(boolean? v) (mlt-properties-set/bool k v)]
        [(anim-property? v)
         (match v
           [(struct* anim-property ([value value]
                                    [position position]
                                    [length length]))
            (cond
              [(string? value)
               (mlt-properties-anim-set mlt-object value position length)]
              [else (error 'video "Anim Property type ~a not currently supported" value)])])]
        [else (error 'video "Property type ~a not currently supported" v)])))
  ;; Attach filters
  (when (service? video)
    (for ([f (in-list (service-filters video))])
      (mlt-service-attach (video-mlt-object video) (video-mlt-object f))))
  ;; Optimise if possible
  #;
  (when (producer? video)
    (mlt-producer-optimise (video-mlt-object video))))

;; Predicate table for determining types
(define global-struct-type-predicate-table '())

;; Constructor for video objects
(define-syntax subclass-empty '(() . ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super*:id ([ids:id default] ...) body ...)
     #:with constructor (format-id stx "make-~a" #'name)
     #:with new-supers (format-id stx "subclass-~a" #'name)
     #:with super (format-id stx "subclass-~a" #'super*)
     #:with predicate (format-id stx "~a?" #'name)
     (define super-vals (syntax-local-value #'super))
     (define all-ids (append (car super-vals) (syntax->datum #'(ids ...))))
     (define all-defaults (append (cdr super-vals) (syntax-e #'(default ...))))
     (quasisyntax/loc stx
       (begin
         (define (constructor #,@(append*
                                  (for/list ([i (in-list all-ids)]
                                             [j (in-list all-defaults)])
                                    `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                      [,(datum->syntax stx i) ,j]))))
           (define object
             (name (let () #f body ...) #,@(map (curry datum->syntax stx) all-ids)))
           (when (video-mlt-object object)
             (finish-mlt-object-init! object))
           object)
         (set! global-struct-type-predicate-table
               (cons (cons predicate #'name) global-struct-type-predicate-table))
         (define-syntax new-supers '#,(cons all-ids all-defaults))))]))

;; Structs
(struct video (mlt-object tag) #:transparent)
(define-constructor video empty ([tag (mutable-set)]))

(struct link video (source target index) #:transparent)
(define-constructor link video ([source #f] [target #f] [index 0])
  (mlt-*-connect target (mlt-*-service source) index)
  (video-mlt-object target))

(struct properties video (prop) #:transparent)
(define-constructor properties video ([prop (hash)]))
(define (properties-ref dict key
                        [default-type 'string])
  (dict-ref (properties-prop dict) key
            (λ ()
              (define v (video-mlt-object dict))
              (unless v
                (error 'properties "MLT object for ~a not created, cannot get default property" dict))
              (match default-type
                ['string (mlt-properties-get v key)]
                ['int (mlt-properties-get-int v key)]
                ['int64 (mlt-properties-get-int64 v key)]
                ['mlt-position (mlt-properties-get-position v key)]
                ['double (mlt-properties-get-double v key)]
                [else (error 'properties "Not a valid default-type ~a" default-type)]))))

(struct anim-property video (value position length) #:transparent)
(define-constructor anim-property video ([value #f] [position #f] [length #f]))

(struct frame properties () #:transparent)
(define-constructor frame properties ())

(struct service properties (filters) #:transparent)
(define-constructor service properties ([filters '()]))

(struct filter service (type source) #:transparent)
(define-constructor filter service ([type #f] [source #f])
  (define f (mlt-factory-filter profile type source))
  (register-mlt-close mlt-filter-close f))

(struct transition service (type source length) #:transparent)
(define-constructor transition service ([type #f] [source #f] [length #f])
  (define t (mlt-factory-transition profile type source))
  (register-mlt-close mlt-transition-close t))

(struct consumer service (type target) #:transparent)
(define-constructor consumer service ([type #f] [target #f])
  (define c (mlt-factory-consumer profile type target))
  (register-mlt-close mlt-consumer-close c))

(struct producer service (type source start end speed seek) #:transparent)
(define-constructor producer service ([type #f] [source #f] [start #f] [end #f] [speed #f] [seek #f])
  (define producer* (mlt-factory-producer profile type source))
  (when (and start end)
    (mlt-producer-set-in-and-out producer* start end))
  (when seek
    (mlt-producer-seek producer* seek))
  (register-mlt-close mlt-producer-close producer*))

(struct playlist producer (elements) #:transparent)
(define-constructor playlist producer ([elements '()])
  (define playlist* (mlt-playlist-init))
  (for ([i (in-list elements)])
    (define i* (video-mlt-object i))
    (match i
      [(struct* playlist-producer ([start start]
                                   [end end]))
       #:when (and start end)
       (mlt-playlist-append-io playlist* i* start end)]
      [(struct* producer ())
       (mlt-playlist-append playlist* i*)]
      [(struct* transition ()) (void)] ;; Must be handled after clips are added
      [(struct* blank ([length length]))
       (mlt-playlist-blank playlist* length)]
      [_ (error 'playlist "Not a playlist element: ~a" i)]))
  (for ([e (in-list elements)]
        [i (in-naturals)])
    (when (transition? e)
      (mlt-playlist-mix playlist*
                        (- i 1)
                        (transition-length e)
                        (video-mlt-object e))))
  (register-mlt-close mlt-playlist-close playlist*))

(struct playlist-producer video (producer start end) #:transparent)
(define-constructor playlist-producer video ([producer #f] [start #f] [end #f])
  (video-mlt-object producer))

(struct blank video (length) #:transparent)
(define-constructor blank video ([length 0]))

(struct multitrack producer (tracks field) #:transparent)
(define-constructor multitrack producer ([tracks '()] [field '()])
  (define tractor* (mlt-tractor-new))                    ; Tractor
  (define multitrack* (mlt-tractor-multitrack tractor*)) ; Multitrack
  (for ([track (in-list tracks)]
        [i (in-naturals)])
    (define track* (video-mlt-object track))
    (mlt-multitrack-connect multitrack* track* i))
  (register-mlt-close mlt-multitrack-close multitrack*)
  (define field* (mlt-tractor-field tractor*))           ; Field
  (for ([element (in-list field)])
    (match element
      [(struct* field-element ([element element]
                               [track track]
                               [track-2 track-2]))
       (define element* (video-mlt-object element))
       (cond
         [(transition? element)
          (mlt-field-plant-transition field* element* track track-2)]
         [(filter? element)
          (mlt-field-plant-filter field* element* track)])]))
  (register-mlt-close mlt-field-close field*)
  tractor*)

(struct field-element video (element track track-2) #:transparent)
(define-constructor field-element video ([element #f] [track #f] [track-2 #f]))
