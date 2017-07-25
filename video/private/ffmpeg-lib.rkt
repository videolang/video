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
         ffi/unsafe/define/conventions)

(define lib-prefix
  (match (system-type 'os)
    ['windows ""]
    [_ "lib"]))

;; The ordering of these libraries matters.
;; This is the order of dependencies so that Racket can
;; resolve it.
(void
 (match (system-type 'os)
   ['macosx (ffi-lib (string-append lib-prefix "openh264") "4")]
   [_ (void)]))
(define avutil-lib
  (ffi-lib (string-append lib-prefix "avutil") "55"))
(define-ffi-definer define-avutil avutil-lib
  #:make-c-id convention:hyphen->underscore)
(define swresample-lib
  (ffi-lib (string-append lib-prefix "swresample") "2"))
(define-ffi-definer define-swresample swresample-lib
  #:make-c-id convention:hyphen->underscore)
(define swscale-lib
  (ffi-lib (string-append lib-prefix "swscale") "4"))
(define-ffi-definer define-swscale swscale-lib
  #:make-c-id convention:hyphen->underscore)
(define avcodec-lib
  (ffi-lib (string-append lib-prefix "avcodec") "57"))
(define-ffi-definer define-avcodec avcodec-lib
  #:make-c-id convention:hyphen->underscore)
(define avformat-lib
  (ffi-lib (string-append lib-prefix "avformat") "57"))
(define-ffi-definer define-avformat avformat-lib
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
  (ffi-lib (string-append lib-prefix "avfilter") "6"))
(define-ffi-definer define-avfilter avfilter-lib
  #:make-c-id convention:hyphen->underscore)

(struct version (major
                 minor
                 bug))
(define (mk-version #:major [major 0]
                    #:minor [minor 0]
                    #:bug [bug 0])
  (version major minor bug))
(define _version
  (make-ctype _int
              (λ (x)
                (integer-bytes->integer
                 (bytes 0
                        (version-major x)
                        (version-minor x)
                        (version-bug x))
                 #f #t))
              (λ (x)
                (define bitstr (integer->integer-bytes x 4 #f #t))
                (mk-version #:major (bytes-ref bitstr 2)
                            #:minor (bytes-ref bitstr 3)
                            #:bug (bytes-ref bitstr 4)))))
