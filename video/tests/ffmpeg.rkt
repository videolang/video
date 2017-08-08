#lang racket

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

(require rackunit
         racket/logging
         racket/pretty
         (prefix-in green: "green.vid")
         (prefix-in video: "../render.rkt")
         (submod "../render.rkt" render-fields)
         (prefix-in video: "../base.rkt")
         "../private/ffmpeg.rkt"
         "../private/ffmpeg-pipeline.rkt"
         "../private/init.rkt"
         "../private/utils.rkt"
         "test-utils.rkt")

(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

;; Give the logging code a dry run to ensure
;;   that it doesn't crash anything.
;; Because this is just the info log, we don't
;;   really care about what is or is not in there.
(with-intercepted-logging (λ (l) (void))
  (λ ()
    (video:render green:vid
                  (make-temporary-file "~a.mp4")
                  #:start 0
                  #:end 30))
  #:logger video-logger
  'debug
  'video)

(let ()
  (define r (make-object video:render% (video:clip vid-mp4)))
  (send r setup (video:make-render-settings #:destination (make-temporary-file "~a.mp4")))
  (define g (get-field render-graph r))
  (render g))
