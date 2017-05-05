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
         "mlt.rkt")

(define init-key "mlt-support-initialized")
(define close-key "mlt-support-closed")
(define counter-key "mlt-support-counter")

(define-ffi-definer define-inside #f)
(define-cpointer-type _custodian)
(define-cpointer-type _custodian-reference)
(define-inside scheme_register_process_global
  (_fun _string _pointer -> _pointer))
(define-inside scheme_add_managed_close_on_exit
  (_fun _custodian/null _racket (_fun _racket _pointer -> _void) _pointer
        -> _custodian-reference/null))

;; Because we currently can't rely on MLT being installed,
;;   only run this module if it is.
(when (ffi-lib? mlt-lib)
  
  ;; Init MLT factory (ONCE PER PROCESS)
  (define counter
    (cast
       (or (scheme_register_process_global counter-key (cast (box 0) _racket _pointer))
           (scheme_register_process_global counter-key #f))
     _pointer
     _racket))

  (unless (scheme_register_process_global init-key (cast 1 _racket _pointer))
    (void (mlt-factory-init #f)))

  ;; Close MLT factory on program exit
  (set-box! counter (add1 (unbox counter)))
  (void
   (scheme_add_managed_close_on_exit
    #f
    counter
    (λ (c d)
      (set-box! counter (sub1 (unbox counter)))
      (when (= (unbox counter) 0)
        (mlt-factory-close)))
    #f)))

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
