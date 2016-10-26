#lang racket

(require ffi/unsafe)

;; This module must be instantiated only once:
(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "mlt-support-initialized"
                                         (cast 1 _scheme _pointer))])
  (when v
    (error "cannot instantiate `video/init' a second time in the same process")))