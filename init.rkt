#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.

(require "private/mlt.rkt"
         ffi/unsafe)

(void (mlt-factory-init #f))

#|
;; This module must be instantiated only once:
(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "GRacket-support-initialized"
                                         (cast 1 _scheme _pointer))])
  (when v
    (error "cannot instantiate `video/init' a second time in the same process")))
|#
