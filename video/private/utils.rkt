#lang racket/base

;; Just a utils file to define some functions that really probably
;;   _should_ be baked into Racket but aren't.
(provide (all-defined-out))
(require racket/contract/base
         racket/runtime-path)

(define-runtime-path video-dir "..")
