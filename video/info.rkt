#lang info

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

(define collection "video")
(define scribblings '(("scribblings/video.scrbl" (multi-page) (language))))
(define raco-commands '(("video"
                         (submod video/raco main)
                         "Preview or Render a Racket Video"
                         #f)))

(define drracket-tools '(("private/tool.rkt")))
(define drracket-tool-names '("Video"))
(define drracket-tool-icons '(#f))

(define test-omit-paths
  '("private/examples.rkt"
    "private/ffmpeg.rkt"
    "private/packetqueue.rkt"
    "private/ffmpeg-stream.rkt"))
