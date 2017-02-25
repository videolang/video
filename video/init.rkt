#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.

(require "private/init-mlt.rkt")
(provide make-video-namespace
         make-video-empty-namespace)

(define-namespace-anchor anchor)

(define (make-video-empty-namespace)
  (let ([ns (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                             'video/init
                             ns)
    (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                             'racket/gui/base
                             ns)
    (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                             'video/core
                             ns)
    ns))

(define (make-video-namespace)
  (let ([ns (make-video-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/base)
      (namespace-require 'racket/gui/base)
      (namespace-require 'racket/class)
      (namespace-require 'video/init)
      (namespace-require 'video/render)
      (namespace-require 'video/player)
      (namespace-require 'video/core))
    ns))
