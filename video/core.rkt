#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(require racket/contract/base
         racket/class
         racket/dict
         "render.rkt"
         "private/video.rkt")

(provide
 (contract-out
  [convert (->* [any/c]
                [#:renderer (or/c (is-a?/c render%))]
                any/c)])
  copy-video
  make-video
  video?
  make-link
  link?
  make-properties
  properties?
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
  make-multitrack
  multitrack?
  make-field-element
  field-element?)
