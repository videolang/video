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
         racket/place)

;; This module provides a place safe semaphore, mutex, and conditinoal variable
;; library.
;; If being place safe is not needed, then using the semaphores
;; provided by racket/base should work fine.

;: WARNING!!!!!
;: WARNING!!!!!
;: WARNING!!!!!
;; This library is NOT compatible with racket's threads. This is ONLY for FFI libraries that
;; are multithreaded.
;;
;; User racket/base's semaphores for Racket threads
;: WARNING!!!!!
;: WARNING!!!!!
;: WARNING!!!!!

(define enabled? (and (place-enabled?)
                      (not (eq? (system-type 'os) 'windows))))

(define-cpointer-type _sema)
(define-cpointer-type _mutex)
(define-cpointer-type _cond)

;; Semaphore
(define sema-create
  (if enabled?
      (get-ffi-obj 'mzrt_sema_create #f
                   (_fun [boxed : (_box _sema/null) = (box #f)]  _int
                         -> [ret : _bool]
                         -> (let ()
                              (when ret
                                (error 'semaphore "Could not create Semaphore"))
                              (unbox boxed))))
      make-semaphore))
(define sema-post
  (if enabled?
      (get-ffi-obj 'mzrt_sema_post #f (_fun _sema -> _int))
      semaphore-post))
(define sema-wait
  (if enabled?
      (get-ffi-obj 'mzrt_sema_wait #f (_fun _sema -> _int))
      semaphore-wait))
(define sema-destroy
  (if enabled?
      (get-ffi-obj 'mzrt_sema_destroy #f (_fun _sema -> _int))
      void))

;; Mutex
(define mutex-create
  (if enabled?
      (get-ffi-obj 'mzrt_mutex_create #f (_fun [boxed : (_box _mutex/null) = (box #f)]
                                               -> [ret : _bool]
                                               -> (let ()
                                                    (when ret
                                                      (error 'mutex "Could not create"))
                                                    (unbox boxed))))
      (λ () (make-semaphore 1))))
(define mutex-lock
  (if enabled?
      (get-ffi-obj 'mzrt_mutex_lock #f (_fun _mutex -> _int))
      semaphore-wait))
(define mutex-trylock
  (if enabled?
      (get-ffi-obj 'mzrt_mutex_trylock #f (_fun _mutex -> _int))
      semaphore-try-wait?))
(define mutex-unlock
  (if enabled?
      (get-ffi-obj 'mzrt_mutex_unlock #f (_fun _mutex -> _int))
      semaphore-post))
(define mutex-destroy
  (if enabled?
      (get-ffi-obj 'mzrt_mutex_destroy #f (_fun _mutex -> _int))
      void))

;; Conditional Variables
(define cond-create
  (if enabled?
      (get-ffi-obj 'mzrt_cond_create #f (_fun [boxed : (_box _cond/null) = (box #f)]
                                              -> [ret : _bool]
                                              -> (let ()
                                                   (when ret
                                                     (error 'cond "Could not create"))
                                                   (unbox boxed))))
      semaphore-peek-evt))
(define cond-wait
  (if enabled?
      (get-ffi-obj 'mzrt_cond_wait #f (_fun _cond _mutex -> _int))
      (λ () (error "Implement me"))))
(define cond-signal
  (if enabled?
      (get-ffi-obj 'mzrt_cond_signal #f (_fun _cond -> _int))
      (λ () (error "Implement me"))))
(define cond-destroy
  (if enabled?
      (get-ffi-obj 'mzrt_cond_destroy #f (_fun _cond -> _int))
      void))
