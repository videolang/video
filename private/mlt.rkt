#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/gui/base)
(provide (all-defined-out))

(define mlt-lib (ffi-lib "libmlt" '("6")))
(define-ffi-definer define-mlt mlt-lib)

;; Types
(define _ibool (make-ctype _bool #f not))
(define _symbol-or-null
  (make-ctype _string
              (λ (v) (if (symbol? v) (symbol->string v) v))
              (λ (v) (if (string? v) (string->symbol v) v))))
(define _mlt-position _int32)
(define _mlt-destructor (_fun _cpointer -> _void))
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
                        ->
                        _ibool)]
   [convert-audio (_fun _mlt-frame-pointer
                        (_ptr io (_ptr io _void))
                        (_ptr io _mlt-audio-format)
                        _mlt-audio-format
                        ->
                        _ibool)]
   [stack-image _mlt-deque]
   [stack-audio _mlt-deque]
   [stack-service _mlt-deque]
   [is-processing _int]))
(define-cstruct (_mlt-service _mlt-properties)
  ([get-frame (_fun _mlt-service-pointer _mlt-frame-pointer _int -> _int)]
   [close _mlt-destructor]
   [close-object (_cpointer/null _void)]
   [local (_cpointer/null _void)]
   [child (_cpointer/null _void)]))
(define-cpointer-type _mlt-consumer _mlt-service-pointer)
(define-cpointer-type _mlt-filter _mlt-service-pointer)
(define-cpointer-type _mlt-transition _mlt-service-pointer)
(define-cpointer-type _mlt-producer _mlt-service-pointer)
(define-cpointer-type _mlt-playlist _mlt-producer)
(define-cpointer-type _mlt-tractor _mlt-producer)
(define-cpointer-type _mlt-multitrack _mlt-producer)
(define-cpointer-type _mlt-field)

;; Factory
(define-mlt mlt-factory-init (_fun _path -> _mlt-repository/null)
  #:c-id mlt_factory_init)
(define-mlt mlt-factory-producer (_fun _mlt-profile-pointer _symbol-or-null _string
                                       -> _mlt-producer/null)
  #:c-id mlt_factory_producer)
(define-mlt mlt-factory-consumer (_fun _mlt-profile-pointer _symbol-or-null _string
                                       -> _mlt-consumer/null)
  #:c-id mlt_factory_consumer)
(define-mlt mlt-factory-filter (_fun _mlt-profile-pointer _symbol-or-null _string -> _mlt-filter/null)
  #:c-id mlt_factory_filter)
(define-mlt mlt-factory-transition (_fun _mlt-profile _symbol-or-null _string -> _mlt-transition/null)
  #:c-id mlt_factory_transition)

;; Profile
(define-mlt mlt-profile-init (_fun _string -> _mlt-profile-pointer/null)
  #:c-id mlt_profile_init)

;; Consumer
(define-mlt mlt-consumer-connect (_fun _mlt-consumer _mlt-service-pointer -> (v : _int)
                                       ->
                                       (cond
                                         [(= 0 v) 'success]
                                         [(= 1 v) 'error/producer-does-not-accept-input]
                                         [(= 2 v) 'error/producer-invalid]
                                         [(= 3 v) 'error/producer-already-registered]
                                         [else    'warning]))
  #:c-id mlt_consumer_connect)
(define-mlt mlt-consumer-start (_fun _mlt-consumer -> _ibool)
  #:c-id mlt_consumer_start)
(define-mlt mlt-consumer-stop (_fun _mlt-consumer -> _ibool)
  #:c-id mlt_consumer_stop)
(define-mlt mlt-consumer-is-stopped (_fun _mlt-consumer -> _bool)
  #:c-id mlt_consumer_is_stopped)

;; Producer
(define-mlt mlt-producer-close (_fun _mlt-producer -> _void)
  #:c-id mlt_producer_close)
(define-mlt mlt-producer-service (_fun _mlt-producer -> _mlt-service-pointer)
  #:c-id mlt_producer_service)
(define-mlt mlt-producer-optimise (_fun _mlt-producer -> _ibool)
  #:c-id mlt_producer_optimise)
(define-mlt mlt-producer-set-in-and-out (_fun _mlt-producer _mlt-position _mlt-position -> _ibool)
  #:c-id mlt_producer_set_in_and_out)
(define-mlt mlt-producer-set-speed (_fun _mlt-producer _double -> _ibool)
  #:c-id mlt_producer_set_speed)

;; Playlist
(define-mlt mlt-playlist-init (_fun -> _mlt-playlist/null)
  #:c-id mlt_playlist_init)
(define-mlt mlt-playlist-append (_fun _mlt-playlist _mlt-producer -> _ibool)
  #:c-id mlt_playlist_append)
(define-mlt mlt-playlist-append-io (_fun _mlt-playlist _mlt-producer _mlt-position _mlt-position ->
                                         _ibool)
  #:c-id mlt_playlist_append_io)
(define-mlt mlt-playlist-close (_fun _mlt-playlist -> _void)
  #:c-id mlt_playlist_close)
(define-mlt mlt-playlist-properties (_fun _mlt-playlist -> _mlt-properties-pointer)
  #:c-id mlt_playlist_properties)
(define-mlt mlt-playlist-producer (_fun _mlt-playlist -> _mlt-producer)
  #:c-id mlt_playlist_producer)
(define-mlt mlt-playlist-blank (_fun _mlt-playlist _mlt-position -> _ibool)
  #:c-id mlt_playlist_blank)
(define-mlt mlt-playlist-count (_fun _mlt-playlist -> _int)
  #:c-id mlt_playlist_count)
(define-mlt mlt-playlist-mix (_fun _mlt-playlist _int _int _mlt-transition -> _ibool)
  #:c-id mlt_playlist_mix)

;; Tractor
(define-mlt mlt-tractor-new (_fun -> _mlt-tractor/null)
  #:c-id mlt_tractor_new)
(define-mlt mlt-tractor-field (_fun _mlt-tractor -> _mlt-field/null)
  #:c-id mlt_tractor_field)
(define-mlt mlt-tractor-multitrack (_fun _mlt-tractor -> _mlt-multitrack)
  #:c-id mlt_tractor_multitrack)

;; MultiTrack
(define-mlt mlt-multitrack-connect (_fun _mlt-multitrack _mlt-producer -> _ibool)
  #:c-id mlt_multitrack_connect)

;; Properties
(define-mlt mlt-properties-set (_fun _mlt-properties-pointer _string _string -> _ibool)
  #:c-id mlt_properties_set)
(define-mlt mlt-properties-set-int (_fun _mlt-properties-pointer _string _int -> _ibool)
  #:c-id mlt_properties_set_int)
(define-mlt mlt-properties-set-int64 (_fun _mlt-properties-pointer _string _int64 -> _ibool)
  #:c-id mlt_properties_set_int64)
(define-mlt mlt-properties-set-position (_fun _mlt-properties-pointer _string _mlt-position -> _ibool)
  #:c-id mlt_properties_set_position)
(define-mlt mlt-properties-set-double (_fun _mlt-properties-pointer _string _double -> _ibool)
  #:c-id mlt_properties_set_double)
(define-mlt mlt-properties-anim-set (_fun _mlt-properties-pointer _string _string _int _int -> _ibool)
  #:c-id mlt_properties_set)

;; Filters
(define-mlt mlt-filter-connect (_fun _mlt-filter _mlt-service-pointer _int -> _ibool)
  #:c-id mlt_filter_connect)
(define-mlt mlt-filter-service (_fun _mlt-filter -> _mlt-service-pointer)
  #:c-id mlt_filter_service)

;; Service
(define-mlt mlt-service-attach (_fun _mlt-service-pointer _mlt-filter -> _ibool)
  #:c-id mlt_service_attach)
