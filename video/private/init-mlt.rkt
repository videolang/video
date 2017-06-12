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

(provide mlt-executor
         register-mlt-close)
(require ffi/unsafe
         ffi/unsafe/define
         "mlt.rkt"
         "threading.rkt")

(define counter-key "mlt-support-counter")
(define counter-type _int)
(define mutex-key "mlt-support-mutex")

(define-ffi-definer define-inside #f)
(define-cpointer-type _custodian)
(define-cpointer-type _custodian-reference)
(define-inside scheme_register_process_global
  (_fun _string _pointer -> _pointer))
(define-inside scheme_add_managed_close_on_exit
  (_fun _custodian/null _racket (_fun _racket _racket -> _void) _racket
        -> _custodian-reference/null))

(define (scheme-install-global! global-key
                                install-proc
                                [free-proc (λ (ptr) (void))]
                                [cast-proc (λ (ptr) ptr)])
  (let ([x (scheme_register_process_global global-key #f)])
      (cond [x (cast-proc x)]
            [else
             (define ptr (install-proc))
             (define installed? (scheme_register_process_global global-key ptr))
             (cond [installed?
                    (free-proc ptr)
                    (cast-proc installed?)]
                   [else ptr])])))

; Get the counter and incrament it
(define counter
  (scheme-install-global! counter-key
                          (λ ()
                            (define ret (malloc 'raw counter-type))
                            (ptr-set! ret counter-type 0)
                            ret)
                          (λ (ptr)
                            (free ptr))))
(define counter-mutex
  (scheme-install-global! mutex-key
                          (λ ()
                            (define s (sema-create 1))
                            s)
                          (λ (sema)
                            (sema-destroy sema))
                          (λ (sema)
                            (cast sema _pointer _sema))))

(define (free-proc c this)
  ;(define x #f)
  (sema-wait counter-mutex)
  (ptr-set! counter counter-type
            (sub1 (ptr-ref counter counter-type)))
  ;(set! x (ptr-ref counter counter-type))
  (when (= (ptr-ref counter counter-type) 0)
    (free counter)
    (mlt-factory-close))
  (sema-post counter-mutex)
  #;(log-error "Dec: ~a" x))


;; Because we currently can't rely on MLT being installed,
;;   only run this module if it is.
(when (ffi-lib? mlt-lib)
  
  ;; Init MLT factory (ONCE PER PROCESS)
  ;(define tmp #f)
  (sema-wait counter-mutex)
  (when (= (ptr-ref counter counter-type) 0)
    (void (mlt-factory-init #f))
    ;; XXX BAD!!! Don't do this!!! (Needs to be replaced!!!)
    (with-handlers ([exn? (λ (e) (void))])
      (define av-register-all (dynamic-require 'video/private/ffmpeg 'av-register-all))
      (av-register-all)
      (define avfilter-register-all (dynamic-require 'video/private/ffmpeg 'avfilter-register-all))
      (avfilter-register-all)
      (define avformat-network-init (dynamic-require 'video/private/ffmpeg 'avformat-network-init))
      (avformat-network-init)))
  (ptr-set! counter counter-type
            (add1 (ptr-ref counter counter-type)))
  ;(set! tmp (ptr-ref counter counter-type) 
  (sema-post counter-mutex)
  #;(log-error "Inc: ~a" tmp)

 
  ;; Close MLT factory on program exit
  (void
   #;
   (scheme_add_managed_close_on_exit
    #f
    #f
    free-proc
    free-proc)))

;; Set up GC thread for MLT objects
(define mlt-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute mlt-executor)
      (loop)))))
;; Register an mlt-object to be closed on garbadge collection
;; For convience, return the mlt-object when done
;; (_mlt-properties -> any) _mlt-properties -> _mlt-properties
(define (register-mlt-close proc v)
  (will-register mlt-executor v (λ (v) (proc v)))
  v)
