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

(require racket/match
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         (for-syntax syntax/parse
                     racket/base))

(define GL-TEXTURE-2D #x0DE1)
(define GL-MAJOR-VERSION #x821B)
(define GL-MINOR-VERSION #x821C)

(define _gl-enum _ufixint)
(define _gl-int _int32)
(define _gl-int64 _int64)
(define _gl-byte _byte)
(define _gl-short _int16)
(define _gl-uint _uint32)
(define _gl-sizei _uint32)
(define _gl-sizei-ptr _intptr)

(define-ffi-definer define-internal #f)

(define gl-get-proc-address
  (match (system-type 'os)
    ['windows
     (define-internal wglGetProcAddress (_fun _string -> _pointer))
     (define-internal LoadLibraryA (_fun _string -> _intptr))
     (define-internal GetProcAddress (_fun _intptr _string -> _pointer))

     (define opengl-module (LoadLibraryA "opengl32.dll"))
     (λ (str type)
       (define maybe-addr (wglGetProcAddress str))
       (define addr
         (if (or (ptr-equal? maybe-addr (ptr-add #f -1))
                 (ptr-equal? maybe-addr (ptr-add #f 0))
                 (ptr-equal? maybe-addr (ptr-add #f 1))
                 (ptr-equal? maybe-addr (ptr-add #f 2))
                 (ptr-equal? maybe-addr (ptr-add #f 3)))
             (GetProcAddress opengl-module str)
             maybe-addr))
       (cast addr _pointer type))]
    ['unix
     (define lib-gl (ffi-lib "libGL" '("1" "")))
     (define-ffi-definer define-libgl lib-gl)
     (define-libgl glXGetProcAddress (_fun _string -> _pointer))
     (define-libgl glXGetProcAddressARB (_fun _string -> _pointer))
     (λ (str type)
       (cast (glXGetProcAddress str) _pointer type))]
    ['macosx
     (λ (str type)
       (get-ffi-obj str #f type))]))

(define-syntax (define-gl stx)
  (syntax-parse stx
    [(_ name type)
     #`(define name (gl-get-proc-address #,(symbol->string (syntax->datum #'name)) type))]))

(define-gl glGetIntegerv (_fun _gl-enum [out : (_ptr o _gl-int)] -> _void -> out))
(define-gl glGetInt64  (_fun _gl-enum [out : (_ptr o _gl-int64)] -> _void -> out))
(define-gl glGetError (_fun -> _gl-enum))
(define-gl glGenVertexArrays (_fun _gl-sizei [out : (_ptr o _gl-uint)] -> _void -> out))
(define-gl glBindVertexArray (_fun _gl-uint -> _void))
(define-gl glGenBuffers (_fun _gl-sizei [out : (_ptr o _gl-uint)] -> _void -> out))
(define-gl glBindBuffers (_fun _gl-uint -> _void))
(define-gl glBufferData (_fun _gl-enum _gl-sizei-ptr _pointer _gl-enum -> _void))
(define-gl glGenTextures (_fun _gl-enum [out : (_ptr o _gl-uint)] -> _void -> out))
(define-gl glBindTextures (_fun _gl-uint -> void))

#|
(require racket/gui/base
         racket/class)
(define f (new frame% [label "hello"]))
(define c (new canvas% [parent f]))
(send c with-gl-context
      (λ ()
        (displayln (glGetIntegerv GL-MAJOR-VERSION))
        (displayln (glGetIntegerv GL-MINOR-VERSION))))
|#
