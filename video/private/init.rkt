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

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/global
         "ffmpeg.rkt"
         "threading.rkt")

(define video-key #"VIDEO-FFMPEG-INIT")

(define-ffi-definer define-internal #f)

(define-internal vasprintf (_fun [out : (_ptr o _string)] _string _pointer -> [ret : _int]
                                 -> (cond
                                      [(< ret 0) (error 'vasprintf "Error ~a" ret)]
                                      [else out])))

#;
(define (vasprintf format args)
  (define-internal vasprintf (_fun [out : (_ptr o _pointer)] _string _pointer -> [ret : _int]
                                   -> (cond
                                        [(< ret 0) (error 'vasprintf "Error ~a" ret)]
                                        [else out])))
  (define pret (vasprintf format args))
  (define ret (cast pret _pointer _string))
  (free pret)
  ret)

#;
(define (callback-proc avcl level fmt args)
  ;(displayln avcl)
  #;(void (vasprintf fmt args)))

 ;; Init ffmpeg (ONCE PER PROCESS)
(void
 (unless (register-process-global video-key (cast 1 _racket _pointer))
   (av-register-all)
   (avfilter-register-all)
   (avformat-network-init)
   #;(av-log-set-callback callback-proc)))
