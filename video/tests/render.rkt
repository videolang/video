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

(require racket/file
         "../render.rkt"
         "../base.rkt"
         (prefix-in mp4: "../render/mp4.rkt")
         (prefix-in png: "../render/png.rkt")
         (prefix-in jpg: "../render/jpg.rkt")
         (prefix-in xml: "../render/xml.rkt")
         "../private/utils.rkt")

(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

(define the-clip
  (playlist
   (clip vid-mp4
         #:start 50
         #:end 100)))

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin mp4:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin png:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin jpg:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin xml:render-mixin)
