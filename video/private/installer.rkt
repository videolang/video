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

;; This module runs on install of video.
;; It checks the version of ffmpeg installed and makes a warning
;;    if the version is old.
;; Video tries to work with old versions of ffmpeg, but newer versions
;;    will work better.
;; Finally, the Racket installation process does not offer an option to
;;    refuse to install. So the installation will happen anyway.
;; This library also prints out the license of the ffmpeg build.

(provide installer)

(require "ffmpeg/lib.rkt"
         "log.rkt")

(define (version-check libname version major minor)
  (unless (and (= (version-major version) major)
               (>= (version-minor version) minor))
    (log-video-warning
     "FFmpeg ~a version is old (~a), consider updating to FFmpeg ~a"
     libname version ffmpeg-rec-version)))

(define (check-license libname license)
  (log-video-info "License for ~a is: ~a" libname license))

(define (installer racket-collects-dir video-collect-dir user? no-modify?)
  (version-check "libavutil" (avutil-version) 55 58)
  (check-license "libavutil" (avutil-license))
  (version-check "libavcodec" (avcodec-version) 57 89)
  (check-license "libavcodec" (avcodec-version))
  (version-check "libavformat" (avformat-version) 57 71)
  (check-license "libavformat" (avformat-license))
  (version-check "libavfilter" (avfilter-version) 6 82)
  (check-license "libavfilter" (avfilter-license))
  (version-check "libswscale" (swscale-version) 4 6)
  (check-license "libswscale" (scscale-license))
  (version-check "libswresample" (swresample-version) 2 7)
  (check-license "libswresample" (swresample-license))
  (version-check "libavdevice" (avdevice-version) 57 6)
  (check-license "libavdevice" (avdevice-license))
  (void))
