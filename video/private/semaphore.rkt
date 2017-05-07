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

(define enabled? (place-enabled?))

(define-cpointer-type _sema)
(define sema-create
  (if enabled?
      (get-ffi-obj 'mzrt_sema_create #f
                   (_fun [boxed : (_box _sema/null) = (box #f)]  _int
                         -> [ret : _int]
                         -> (values (unbox boxed) ret)))
      (λ (count) (values 0 0))))
(define sema-post
  (if enabled?
      (get-ffi-obj 'mzrt_sema_post #f (_fun _sema -> _int))
      (λ (sema) 0)))
(define sema-wait
  (if enabled?
      (get-ffi-obj 'mzrt_sema_wait #f (_fun _sema -> _int))
      (λ (sema) 0)))

(define sema-destroy
  (if enabled?
      (get-ffi-obj 'mzrt_sema_destroy #f (_fun _sema -> _int))
      (λ (sema) 0)))
