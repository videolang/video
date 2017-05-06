#lang racket/base

(provide (all-defined-out))
(require ffi/unsafe
         racket/place)

(define enabled? (place-enabled?))

(define-cpointer-type _sema)
(define sema-create
  (if enabled?
      (get-ffi-obj 'mzrt_sema_create #f
                   (_fun [boxed : (_box _sema/null) = (box #f)]  _int
                         -> [ret : _int]
                         -> (values (unbox boxed) ret)))
      (λ (count) (values 0 0))))
(define sema-post
  (if enabled?
      (get-ffi-obj 'mzrt_sema_post #f (_fun _sema -> _int))
      (λ (sema) 0)))
(define sema-wait
  (if enabled?
      (get-ffi-obj 'mzrt_sema_wait #f (_fun _sema -> _int))
      (λ (sema) 0)))

(define sema-destroy
  (if enabled?
      (get-ffi-obj 'mzrt_sema_destroy #f (_fun _sema -> _int))
      (λ (sema) 0)))
