#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.
;;   If you do this though, do make sure to shut things down with mlt-factory-init

(provide mlt-executor
         register-mlt-close)
(require "mlt.rkt"
         "once.rkt")

;; Init MLT factory
(void (mlt-factory-init #f))

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
