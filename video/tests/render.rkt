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
         racket/port
         racket/class
         "../render.rkt"
         "../base.rkt"
         "../private/utils.rkt"
         "../private/debug-mixin.rkt")

(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

(define the-clip
  (playlist
   (clip vid-mp4
         #:properties (hash "start" 50
                            "end" 100))))

(render the-clip
        (make-temporary-file "vidtest~a.mp4"))

(render the-clip
        (make-temporary-file "vidtest~a.mp4")
        #:render-mixin cmdline-mixin)

(let ()
  (define breaks
    (multitrack
     (color "green")
     (composite-merge 0 0 1/2 1/2)
     (color "red")))
  (render breaks (make-temporary-file "color~a.mov")
          #:width 1280
          #:height 720
          #:start 0
          #:end 2
          #:fps 24))

(parameterize ([current-output-port (open-output-nowhere)])
  (render/pretty (multitrack
                  (color "green")
                  (clip vid-mp4))
                 (make-temporary-file "~a.mp4")))

(let ()
  (define video
    (color "green"
           #:properties (hash "start" 10
                              "end" 20)))
  (define render (make-object render% video))
  (send render setup (make-render-settings #:destination (make-temporary-file "tmp~a.mp4")))
  (send render start-rendering)
  (sleep 0.1)
  (send render stop-rendering))

(let ()
  (define video
    (color "green"
           #:properties (hash "start" 1
                              "end" 1)))
  (define render (make-object render% video))
  (send render setup (make-render-settings #:destination (make-temporary-file "tmp~a.mp4")))
  (send render start-rendering)
  (sleep 0.1)
  (send render stop-rendering))
