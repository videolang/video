#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.

(require "private/mlt.rkt")

(void (mlt-factory-init #f))
