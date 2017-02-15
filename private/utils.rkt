#lang racket/base

;; Just a utils file to define some functions that really probably
;;   _should_ be baked into Racket but aren't.
(provide (all-defined-out))
(require racket/contract/base
         racket/runtime-path)

;; Contract to see if a number is a nonnegative integer
;; (-> any/c boolean?)
;; Given: 0 Expect: #t
;; Given: 5 Expect: #t
;; Given: -1 Expect: #f
;; Given: 2.5 Expect: #f
(define nonnegative-integer?
  (or/c (and/c (>=/c 0) integer?)))

(define-runtime-path video-dir "..")
