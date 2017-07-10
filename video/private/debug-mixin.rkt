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


;; This module contains a debug mixin for the video/render class so that it can
;;   use the render/cmdline-ffmpeg proc.
;; WARNING!!!: Like render/cmdline-ffmpeg, this mixin must not be used outside of
;;   debuging Video itself.

(provide (all-defined-out))
(require racket/class
         racket/contract/base
         graph
         (prefix-in ffmpeg: "ffmpeg-pipeline.rkt")
         (prefix-in base: racket/base)
         "../render.rkt"
         (submod "../render.rkt" render-fields))

(define (cmdline-mixin %)
  (class* % (render<%>)
    (super-new)
    (inherit-field render-graph)
    (define/override (feed-buffers) (void))
    (define/override (write-output)
      (ffmpeg:render/cmdline-ffmpeg render-graph))))
