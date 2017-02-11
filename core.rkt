#lang racket/base

(require racket/contract/base
         racket/class
         "render.rkt"
         "private/video.rkt")
(provide
 (contract-out
  [convert (->* [any/c]
                [#:renderer (or/c (is-a?/c render%))]
                any/c)])
  make-video
  video?
  make-link
  link?
  make-properties
  properties?
  make-anim-property
  anim-property?
  make-frame
  frame?
  make-service
  service?
  make-filter
  filter?
  make-transition
  transition?
  make-consumer
  consumer?
  make-producer
  producer?
  make-blank
  blank?
  make-playlist
  playlist?
  make-playlist-producer
  playlist-producer?
  make-multitrack
  multitrack?
  make-field-element
  field-element?)
