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
         "mlt.rkt")

(define init-key "mlt-support-initialized")
(define close-key "mlt-support-closed")

;; Because we currently can't rely on MLT being installed,
;;   only run this module if it is.
(when (ffi-lib? mlt-lib)
  ;; Init MLT factory (ONCE PER PROCESS)
  (define scheme_register_process_global
    (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

  (unless (scheme_register_process_global init-key (cast 1 _racket _pointer))
    (void (mlt-factory-init #f)))

  (when (scheme_register_process_global close-key #f)
    (error "MLT support already closed for this process"))

  ;; Close MLT factory on program exit
  (unless (scheme_register_process_global close-key (case 1 _racket _pointer))
    (void ((get-ffi-obj 'atexit #f (_fun (_fun -> _void) -> _bool))
           (function-ptr
            mlt-factory-close
            (_fun -> _void)))))
  #;
  (void (plumber-add-flush!
         (current-plumber)
         (λ (p)
           (collect-garbage)
           (scheme_register_process_global close-key (cast 1 _racket _pointer))
           (mlt-factory-close)))))

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
