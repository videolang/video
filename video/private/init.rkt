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

(provide (all-defined-out))
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/global
         "ffmpeg/libvid.rkt"
         "ffmpeg/main.rkt")

(define video-key #"VIDEO-FFMPEG-INIT")

(define-ffi-definer define-internal #f)

(define MAX-LOG-MSG-SIZE 4096)

(define (callback-proc avcl level msg)
  (displayln msg)
  (void))

 ;; Init ffmpeg (ONCE PER PROCESS)
(when (ffmpeg-installed?)
  (unless (register-process-global video-key (cast 1 _racket _pointer))
    (av-register-all)
    (avfilter-register-all)
    (avformat-network-init)
    (avdevice-register-all)
    #;(when (libvid-installed?)
      (set-racket-log-callback callback-proc)
      (av-log-set-callback ffmpeg-log-callback))))

;; Because portaudio has a nasty tendency to output a lot of garbadge to stdout, only
;; require it in situations where its actually needed.
(module* portaudio racket
  (require ffi/unsafe
           ffi/unsafe/global
           portaudio)
  
  (define portaudio-key #"VIDEO-PORTAUDIO-INIT")
  (unless (register-process-global portaudio-key (cast 1 _racket _pointer))
    (pa-maybe-initialize)))
