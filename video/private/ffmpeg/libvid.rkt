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

(provide (all-defined-out))
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         "../log.rkt"
         "constants.rkt"
         "data.rkt"
         "lib.rkt")

(define (libvid-not-installed)
  (log-video-warning "libvideo NOT installed, cannot capture ffmpeg log messages.")
  #f)

(define (((error-libvid-not-installed name)) . rest)
  (error name "libvid not installed, cannot use this function"))

(define libvid-lib
  (ffi-lib (string-append "libvid") "0"
           #:fail libvid-not-installed))
(define-ffi-definer define-libvid libvid-lib
  #:default-make-fail error-libvid-not-installed
  #:make-c-id convention:hyphen->underscore)

(define racket-log-callback-box (box #f))
(define-libvid set-racket-log-callback
  (_fun (_fun #:async-apply (λ (x) (x))
              #:keep malloc-immobile-cell
              _av-log-constant _av-bprint-pointer _av-bprint-pointer
              -> _void) -> _void))
(define-libvid ffmpeg-log-callback _fpointer)

(define-libvid libvid-get-version-major (_fun -> _int))
(define-libvid libvid-get-version-minor (_fun -> _int))
(define-libvid libvid-get-version-patch (_fun -> _int))
(define-libvid libvid-get-version-prerelease (_fun -> _int))

;; Determin if libvid is installed.
;; -> boolean?
(define (libvid-installed?)
  libvid-lib)

;; Get the current version of libvid.
;; This should match the current version of video, but could be off by
;;   the patch number.
(define (libvid-version)
  (mk-version #:major (libvid-get-version-major)
              #:minor (libvid-get-version-minor)
              #:patch (libvid-get-version-patch)))
