#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.
;;   If you do this though, do make sure to shut things down with mlt-factory-init

(provide mlt-executor
         register-mlt-close)
(require ffi/unsafe
         "mlt.rkt")

;; Init MLT factory (ONCE PER PROCESS)
(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "mlt-support-initialized"
                                         (cast 1 _racket _pointer))])
  (unless v
    (void (mlt-factory-init #f))))

;; Close MLT factory on program exit
(void
 (plumber-add-flush!
  (current-plumber) (λ (p)
                      (collect-garbage)
                      (mlt-factory-close))))

;; Set up GC thread for MLT objects
(define mlt-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute mlt-executor)
      (loop)))))
;; Register an mlt-object to be closed on garbadge collection
;; For convience, return the mlt-object when done
;; (_mlt-properties -> any) _mlt-properties -> _mlt-properties
(define (register-mlt-close proc v)
  (will-register mlt-executor v (λ (v) (proc v)))
  v)
