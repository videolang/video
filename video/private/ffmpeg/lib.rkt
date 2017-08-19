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
(require racket/match
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         "../log.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket/base))

(struct exn:ffmpeg exn ())
(struct exn:ffmpeg:lib exn:ffmpeg ())
(struct exn:ffmpeg:again exn:ffmpeg ())
(struct exn:ffmpeg:eof exn:ffmpeg ())
(struct exn:ffmpeg:flush exn:ffmpeg ())
(struct exn:ffmpeg:stream-not-found exn:ffmpeg ())
(struct exn:ffmpeg:decoder-not-found exn:ffmpeg ())
(struct exn:ffmpeg:version exn:ffmpeg ())

;; ===================================================================================================

(define ffmpeg-min-version "3.2")
(define ffmpeg-rec-version "3.3")

(define lib-prefix
  (match (system-type 'os)
    ['windows ""]
    [_ "lib"]))

(define (ffmpeg-not-installed)
  (log-video-error "FFmpeg NOT installed.")
  #f)

(define (((error-not-installed name)) . rest)
  (error name "FFmpeg not installed"))

;; The ordering of these libraries matters.
;; This is the order of dependencies so that Racket can
;; resolve it.
(void
 (match (system-type 'os)
   ['macosx (ffi-lib (string-append lib-prefix "openh264") "4")]
   [_ (void)]))
(define avutil-lib
  (ffi-lib (string-append lib-prefix "avutil") "55"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-avutil avutil-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(define swresample-lib
  (ffi-lib (string-append lib-prefix "swresample") "2"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-swresample swresample-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(define swscale-lib
  (ffi-lib (string-append lib-prefix "swscale") "4"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-swscale swscale-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(define avcodec-lib
  (ffi-lib (string-append lib-prefix "avcodec") "57"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-avcodec avcodec-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(define avformat-lib
  (ffi-lib (string-append lib-prefix "avformat") "57"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-avformat avformat-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(match (system-type 'os)
  ['windows
   (define postproc-lib
     (ffi-lib (string-append lib-prefix "postproc") "54"))
   (define-ffi-definer define-postproc postproc-lib
     #:make-c-id convention:hyphen->underscore)
   (void)]
  [_ (void)])
(define avfilter-lib
  (ffi-lib (string-append lib-prefix "avfilter") "6"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-avfilter avfilter-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)
(define avdevice-lib
  (ffi-lib (string-append lib-prefix "avdevice") "57"
           #:fail ffmpeg-not-installed))
(define-ffi-definer define-avdevice avdevice-lib
  #:default-make-fail error-not-installed
  #:make-c-id convention:hyphen->underscore)

;; ===================================================================================================

(struct version (major
                 minor
                 bug)
  #:methods gen:custom-write
  [(define (write-proc x port mode)
   (fprintf port "~a.~a.~a"
            (version-major x)
            (version-minor x)
            (version-bug x)))])
(define (mk-version #:major [major 0]
                    #:minor [minor 0]
                    #:bug [bug 0])
  (version major minor bug))
(define _version
  (make-ctype _uint
              (λ (x)
                (integer-bytes->integer
                 (bytes 0
                        (version-major x)
                        (version-minor x)
                        (version-bug x))
                 #f #t))
              (λ (x)
                (define bitstr (integer->integer-bytes x 4 #f #t))
                (mk-version #:major (bytes-ref bitstr 1)
                            #:minor (bytes-ref bitstr 2)
                            #:bug (bytes-ref bitstr 3)))))

;; ===================================================================================================

(define-avutil avutil-version (_fun -> _version))
(define-avutil avutil-license (_fun -> _string))
(define-avutil avutil-configuration (_fun -> _string))
(define-swresample swresample-version (_fun -> _version))
(define-swresample swresample-license (_fun -> _string))
(define-swresample swresample-configuration (_fun -> _string))
(define-swscale swscale-version (_fun -> _version))
(define-swscale swscale-license (_fun -> _string))
(define-swscale swscale-configuration (_fun -> _string))
(define-avcodec avcodec-version (_fun -> _version))
(define-avcodec avcodec-license (_fun -> _string))
(define-avcodec avcodec-configuration (_fun -> _string))
(define-avformat avformat-version (_fun -> _version))
(define-avformat avformat-license (_fun -> _string))
(define-avformat avformat-configuration (_fun -> _string))
(define-avfilter avfilter-version (_fun -> _version))
(define-avfilter avfilter-license (_fun -> _string))
(define-avfilter avfilter-configuration (_fun -> _string))
(define-avdevice avdevice-version (_fun -> _version))
(define-avdevice avdevice-license (_fun -> _string))
(define-avdevice avdevice-configuration (_fun -> _string))

;; ===================================================================================================

(define (ffmpeg-installed?)
  (and avutil-lib
       swresample-lib
       swscale-lib
       avcodec-lib
       avfilter-lib
       avformat-lib
       avdevice-lib))

;; Does a check to ensure that the correct version of FFmpeg is installed. If an invalid
;;   version is installed, return #f, otherwise return #t. To be a valid version the `major` field
;;   must match EXACTLY, and the minor field must be AT LEAST the specified version.
;; Version Int Int -> Boolean
(define (version-check libname version major minor)
  (and (= (version-major version) major)
               (>= (version-minor version) minor)))

;; Check to ensure that ffmpeg meets the minimum required version.
;; returns #t if it does, otherwise returns #f.
;; -> Boolean
(define (ffmpeg-min-version?)
  (and (version-check "libavutil" (avutil-version) 55 34)
       (version-check "libavcodec" (avcodec-version) 57 64)
       (version-check "libavformat" (avformat-version) 57 56)
       (version-check "libavfilter" (avfilter-version) 6 65)
       (version-check "libswscale" (swscale-version) 4 2)
       (version-check "libswresample" (swresample-version) 2 3)
       (version-check "libavdevice" (avdevice-version) 57 1)))

;; Test to see if FFMPEG meets the recommended versions.
;; Video should work with versions lower than this, but will not
;;   perform as well
;; -> Boolean
(define (ffmpeg-recommended-version?)
  (and (version-check "libavutil" (avutil-version) 55 58)
       (version-check "libavcodec" (avcodec-version) 57 89)
       (version-check "libavformat" (avformat-version) 57 71)
       (version-check "libavfilter" (avfilter-version) 6 82)
       (version-check "libswscale" (swscale-version) 4 6)
       (version-check "libswresample" (swresample-version) 2 7)
       (version-check "libavdevice" (avdevice-version) 57 6)))
