#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/gui/base
         (for-syntax racket/syntax
                     racket/base
                     syntax/parse))
(provide (all-defined-out))

;; MLT Library
(define mlt-lib (ffi-lib "libmlt" '("6")))
(define-ffi-definer define-mlt mlt-lib)

;; MLT Error
(struct exn:fail:mlt exn:fail ())
(struct exn:fail:mlt:warning exn:fail:mlt ())
(define raise-mlt-warnings (make-parameter #f))
(define-syntax (raise-mlt-warning stx)
  (syntax-parse stx
    [(_ function:id)
     (syntax/loc stx
       (when (raise-mlt-warnings)
         (raise (exn:fail:mlt:warning (format "MLT: Function ~a returned warning" 'function)
                                      (current-continuation-marks)))))]))
(define-syntax (raise-mlt-error stx)
  (syntax-parse stx
    [(_ function:id (~optional (~seq #:msg msg:str)
                               #:defaults ([msg #'""])))
     (syntax/loc stx
       (raise (exn:fail:mlt (format "MLT: Function ~a failed: ~a" 'function msg)
                            (current-continuation-marks))))]))

;; Types
(define _symbol-or-null
  (make-ctype _string
              (λ (v) (if (symbol? v) (symbol->string v) v))
              (λ (v) (if (string? v) (string->symbol v) v))))
(define _mlt-position _int32)
(define _mlt-destructor (_fun (_cpointer 'destruct) -> _void))
(define _mlt-image-format (_enum '(mlt-image-none = 0
                                   mlt-image-rgb24
                                   mlt-image-rgb24a
                                   mlt-image-yuv422
                                   mlt-image-yuv420p
                                   mlt-image-opengl
                                   mlt-image-glsl
                                   mlt-image-glsl-texture)))
(define _mlt-audio-format (_enum '(mlt-audio-none = 0
                                   mlt-audio-pcm
                                   mlt-audio-s16
                                   mlt-audio-s32
                                   mlt-audio-float
                                   mlt-audio-s32le
                                   mlt-audio-f32le
                                   mlt-audio-u8)))
(define-cpointer-type _mlt-repository)
(define-cpointer-type _mlt-deque)
(define-cpointer-type _playlist-entry)
(define-cpointer-type _mlt-event)
(define-cpointer-type _mlt-field)
(define-cstruct _mlt-profile
  ([description _string]
   [frame-rate-num _int]
   [frame-rate-den _int]
   [width _int]
   [height _int]
   [progressive _int]
   [sample-aspect-num _int]
   [sample-aspect-den _int]
   [display-aspect-num _int]
   [disaply-aspect-den _int]
   [colorspace _int]
   [is-explicit _int]))
(define-cstruct _mlt-properties
  ([child (_cpointer/null 'child)]
   [local (_cpointer/null 'local)]
   [close _mlt-destructor]
   [close-object (_cpointer/null 'close-object)]))
(define-cstruct (_mlt-frame _mlt-properties)
  ([get-alpha-mask (_fun _mlt-frame-pointer -> (_ptr o _uint8))]
   [convert-image (_fun _mlt-frame-pointer
                        (_ptr io (_ptr io _uint8))
                        (_ptr io _mlt-image-format)
                        _mlt-image-format
                        -> [v : _bool]
                        -> (when v (raise-mlt-error mlt-frame-convert-image)))]
   [convert-audio (_fun _mlt-frame-pointer
                        (_ptr io (_ptr io _void))
                        (_ptr io _mlt-audio-format)
                        _mlt-audio-format
                        -> [v : _bool]
                        -> (when v (raise-mlt-error mlt-framework-convert-audio)))]
   [stack-image _mlt-deque]
   [stack-audio _mlt-deque]
   [stack-service _mlt-deque]
   [is-processing _int]))
(define-cstruct (_mlt-service _mlt-properties)
  ([get-frame (_fun _mlt-service-pointer _mlt-frame-pointer _int -> _int)]
   [close* _mlt-destructor]
   [close-object (_cpointer/null 'class-object)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-consumer _mlt-service)
  ([start* (_fun _mlt-consumer-pointer -> [v : _bool]
                 -> (when v (raise-mlt-error mlt-consumer-start)))]
   [stop* (_fun _mlt-consumer-pointer -> [v : _bool]
                -> (when v (raise-mlt-error mlt-consumer-stop)))]
   [is-stopped* (_fun _mlt-consumer-pointer -> [v : _bool]
                      -> (when v (raise-mlt-error mlt-consumer-is-stopped)))]
   [purge* (_fun _mlt-consumer-pointer -> [v : _bool]
                 -> (when v (raise-mlt-error mlt-consumer-purge)))]
   [close* (_fun _mlt-consumer-pointer -> _void)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-filter _mlt-service)
  ([close* (_fun _mlt-filter-pointer -> _void)]
   [process* (_fun _mlt-filter-pointer _mlt-frame-pointer -> _mlt-frame-pointer)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-transition _mlt-service)
  ([close* (_fun _mlt-transition-pointer -> _void)]
   [process* (_fun _mlt-transition-pointer _mlt-frame-pointer _mlt-frame-pointer
                   -> _mlt-frame-pointer)]
   [child (_cpointer/null 'child)]
   [producer _mlt-service-pointer]
   [frames (_cpointer/null 'frames _mlt-frame-pointer)]
   [held _int]))
(define-cstruct (_mlt-producer _mlt-service)
  ([get-frame* (_fun _mlt-producer-pointer _mlt-frame-pointer _int -> _int)]
   [close* _mlt-destructor]
   [close-object (_cpointer/null 'close-object)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-playlist _mlt-producer)
  ([blank* _mlt-producer]
   [size* _int]
   [count* _int]
   [list* (_cpointer 'list (_cpointer 'list* _playlist-entry))]))
(define-cstruct (_mlt-tractor _mlt-producer)
  ([producer _mlt-service-pointer]))
(define-cstruct _mlt-track
  ([producer _mlt-producer-pointer/null]
   [event _mlt-event/null]))
(define-cstruct (_mlt-multitrack _mlt-producer)
   ([list* _mlt-track-pointer]
   [size* _int]
   [count* _int]))

;; Factory
(define-mlt mlt-factory-init (_fun _path -> _mlt-repository/null)
  #:c-id mlt_factory_init)
(define-mlt mlt-factory-producer (_fun _mlt-profile-pointer _symbol-or-null _string
                                       -> _mlt-producer-pointer/null)
  #:c-id mlt_factory_producer)
(define-mlt mlt-factory-consumer (_fun _mlt-profile-pointer _symbol-or-null _string
                                       -> _mlt-consumer-pointer/null)
  #:c-id mlt_factory_consumer)
(define-mlt mlt-factory-filter (_fun _mlt-profile-pointer _symbol-or-null _string
                                     -> _mlt-filter-pointer/null)
  #:c-id mlt_factory_filter)
(define-mlt mlt-factory-transition (_fun _mlt-profile-pointer _symbol-or-null _string
                                         -> _mlt-transition-pointer/null)
  #:c-id mlt_factory_transition)

;; Profile
(define-mlt mlt-profile-init (_fun _string -> _mlt-profile-pointer/null)
  #:c-id mlt_profile_init)

;; Consumer
(define-mlt mlt-consumer-connect (_fun _mlt-consumer-pointer _mlt-service-pointer -> (v : _int)
                                       ->
                                       (cond
                                         [(= 0 v) 'success]
                                         [(= 1 v)
                                          (raise-mlt-error mlt-consumer-connect
                                                           #:msg "Producer does not accept input")]
                                         [(= 2 v) (raise-mlt-error mlt-consumer-connect
                                                                   #:msg "Invalid Producer")]
                                         [(= 3 v) (raise-mlt-error
                                                   mlt-consumer-connect
                                                   #:msg "Producer has already been registered")]
                                         [else (raise-mlt-warning mlt-consumer-connect)]))
  #:c-id mlt_consumer_connect)
(define-mlt mlt-consumer-start (_fun _mlt-consumer-pointer -> [v : _bool]
                                     -> (when v (raise-mlt-error mlt-consumer-start)))
  #:c-id mlt_consumer_start)
(define-mlt mlt-consumer-stop (_fun _mlt-consumer-pointer -> [v : _bool]
                                    -> (when v (raise-mlt-error mlt-consumer-stop)))
  #:c-id mlt_consumer_stop)
(define-mlt mlt-consumer-is-stopped (_fun _mlt-consumer-pointer -> _bool)
  #:c-id mlt_consumer_is_stopped)

;; Producer
(define-mlt mlt-producer-close (_fun _mlt-producer-pointer -> _void)
  #:c-id mlt_producer_close)
(define-mlt mlt-producer-service (_fun _mlt-producer-pointer -> _mlt-service-pointer)
  #:c-id mlt_producer_service)
(define-mlt mlt-producer-optimise (_fun _mlt-producer-pointer -> [v : _bool]
                                        -> (when v (raise-mlt-error mlt-producer-optimise)))
  #:c-id mlt_producer_optimise)
(define-mlt mlt-producer-set-in-and-out (_fun _mlt-producer-pointer _mlt-position _mlt-position
                                              -> [v : _bool]
                                              -> (when v
                                                   (raise-mlt-error mlt-producer-set-in-and-out)))
  #:c-id mlt_producer_set_in_and_out)
(define-mlt mlt-producer-set-speed (_fun _mlt-producer-pointer _double -> [v : _bool]
                                         -> (when v (raise-mlt-error mlt-producer-set-speed)))
  #:c-id mlt_producer_set_speed)

;; Playlist
(define-mlt mlt-playlist-init (_fun -> _mlt-playlist-pointer/null)
  #:c-id mlt_playlist_init)
(define-mlt mlt-playlist-append (_fun _mlt-playlist-pointer _mlt-producer-pointer -> [v : _bool]
                                      -> (when v (raise-mlt-error mlt-playlist-append)))
  #:c-id mlt_playlist_append)
(define-mlt mlt-playlist-append-io (_fun _mlt-playlist-pointer
                                         _mlt-producer-pointer
                                         _mlt-position
                                         _mlt-position
                                         -> [v : _bool]
                                         -> (when v (raise-mlt-error mlt-playlist-append-io)))
  #:c-id mlt_playlist_append_io)
(define-mlt mlt-playlist-close (_fun _mlt-playlist-pointer -> _void)
  #:c-id mlt_playlist_close)
(define-mlt mlt-playlist-properties (_fun _mlt-playlist-pointer -> _mlt-properties-pointer)
  #:c-id mlt_playlist_properties)
(define-mlt mlt-playlist-producer (_fun _mlt-playlist-pointer -> _mlt-producer-pointer)
  #:c-id mlt_playlist_producer)
(define-mlt mlt-playlist-blank (_fun _mlt-playlist _mlt-position -> [v : _bool]
                                     -> (when v (raise-mlt-error mltt-playlist-blank)))
  #:c-id mlt_playlist_blank)
(define-mlt mlt-playlist-count (_fun _mlt-playlist -> _int)
  #:c-id mlt_playlist_count)
(define-mlt mlt-playlist-mix (_fun _mlt-playlist _int _int _mlt-transition -> [v : _bool]
                                   -> (when v (raise-mlt-error mlt-playlist-mix)))
  #:c-id mlt_playlist_mix)

;; Tractor
(define-mlt mlt-tractor-new (_fun -> _mlt-tractor-pointer/null)
  #:c-id mlt_tractor_new)
(define-mlt mlt-tractor-field (_fun _mlt-tractor-pointer -> _mlt-field/null)
  #:c-id mlt_tractor_field)
(define-mlt mlt-tractor-multitrack (_fun _mlt-tractor-pointer -> _mlt-multitrack-pointer)
  #:c-id mlt_tractor_multitrack)

;; MultiTrack
(define-mlt mlt-multitrack-connect (_fun _mlt-multitrack-pointer _mlt-producer-pointer -> [v : _bool]
                                         -> (when v (raise-mlt-error mlt-multitrack-connect)))
  #:c-id mlt_multitrack_connect)

;; Properties
(define-mlt mlt-properties-set (_fun _mlt-properties-pointer _string _string -> [v : _bool]
                                     -> (when v (raise-mlt-error mlt-properties-set)))
  #:c-id mlt_properties_set)
(define-mlt mlt-properties-set-int (_fun _mlt-properties-pointer _string _int -> [v : _bool]
                                         -> (when v (raise-mlt-error mlt-properties-set-int)))
  #:c-id mlt_properties_set_int)
(define-mlt mlt-properties-set-int64 (_fun _mlt-properties-pointer _string _int64 -> [v : _bool]
                                           -> (when v (raise-mlt-error mlt-properties-set-int64)))
  #:c-id mlt_properties_set_int64)
(define-mlt mlt-properties-set-position (_fun _mlt-properties-pointer _string _mlt-position
                                              -> [v : _bool]
                                              -> (when v
                                                   (raise-mlt-error mlt-properties-set-position)))
  #:c-id mlt_properties_set_position)
(define-mlt mlt-properties-set-double (_fun _mlt-properties-pointer _string _double
                                            -> [v : _bool]
                                            -> (when v (raise-mlt-error mlt-properties-set-double)))
  #:c-id mlt_properties_set_double)
(define-mlt mlt-properties-anim-set (_fun _mlt-properties-pointer _string _string _int _int
                                          -> [v : _bool]
                                          -> (when v (raise-mlt-error mlt-properties-anim-set)))
  #:c-id mlt_properties_set)
(define-mlt mlt-properties-get-name (_fun _mlt-properties-pointer _int -> _string)
  #:c-id mlt_properties_get_name)
(define-mlt mlt-properties-get (_fun _mlt-properties-pointer _string -> _string)
  #:c-id mlt_properties_get)
(define-mlt mlt-properties-get-int (_fun _mlt-properties-pointer _string -> _int)
  #:c-id mlt_properties_get_int)
(define-mlt mlt-properties-get-int64 (_fun _mlt-properties-pointer _string -> _int64)
  #:c-id mlt_properties_get_int64)
(define-mlt mlt-properties-get-position (_fun _mlt-properties-pointer _string -> _mlt-position)
  #:c-id mlt_properties_set_position)
(define-mlt mlt-properties-get-double (_fun _mlt-properties-pointer _string -> _double)
  #:c-id mlt_properties_get_double)
(define-mlt mlt-properties-save (_fun _mlt-properties-pointer _path -> [v : _bool]
                                      -> (when v (raise-mlt-error mlt-properties-save)))
  #:c-id mlt_properties_save)
(define-mlt mlt-properties-count (_fun _mlt-properties-pointer -> _int)
  #:c-id mlt_properties_count)


;; Filters
(define-mlt mlt-filter-connect (_fun _mlt-filter-pointer _mlt-service-pointer _int
                                     -> [v : _bool]
                                     -> (when v (raise-mlt-error mlt-filter-connect)))
  #:c-id mlt_filter_connect)
(define-mlt mlt-filter-service (_fun _mlt-filter-pointer -> _mlt-service-pointer)
  #:c-id mlt_filter_service)

;; Service
(define-mlt mlt-service-attach (_fun _mlt-service-pointer _mlt-filter-pointer -> [v : _bool]
                                     -> (when v (raise-mlt-error mlt-service-attach)))
  #:c-id mlt_service_attach)
