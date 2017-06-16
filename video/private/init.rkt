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

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.
;;   If you do this though, do make sure to shut things down with mlt-factory-init

(provide video-executor
         register-video-close)
(require ffi/unsafe
         ffi/unsafe/global
         "ffmpeg.rkt"
         "threading.rkt")

(define video-key #"VIDEO-FFMPEG-INIT")

 ;; Init ffmpeg (ONCE PER PROCESS)
(void
 (unless (register-process-global video-key (cast 1 _racket _pointer))
   (av-register-all)
   (avfilter-register-all)
   (avformat-network-init)))

;; Set up GC thread for Video objects
(define video-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute video-executor)
      (loop)))))

;; Register an video-object to be closed on garbadge collection
;; For convience, return the mlt-object when done
;; (_mlt-properties -> any) _mlt-properties -> _mlt-properties
(define (register-video-close proc v)
  (will-register video-executor v (λ (v) (proc v)))
  v)
