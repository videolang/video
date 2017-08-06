#lang racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions)

(define-ffi-definer define-internal #f)

(define-internal wglGetProcAddress (_fun _string -> _pointer))
(define-internal LoadLibraryA (_fun _string -> _intptr))
(define-internal GetProcAddress (_fun _intptr _string -> _pointer))

(define GL-MAJOR-VERSION #x821B)
(define GL-MINOR-VERSION #x821C)

(define opengl-module (LoadLibraryA "opengl32.dll"))
(define (gl-get-proc-address str type)
  (define maybe-addr (wglGetProcAddress str))
  (define addr
    (if (or (ptr-equal? maybe-addr (ptr-add #f -1))
            (ptr-equal? maybe-addr (ptr-add #f 0))
            (ptr-equal? maybe-addr (ptr-add #f 1))
            (ptr-equal? maybe-addr (ptr-add #f 2))
            (ptr-equal? maybe-addr (ptr-add #f 3)))
        (GetProcAddress opengl-module str)
        maybe-addr))
  (cast addr _pointer type))

(define gl-get-integerv
   (gl-get-proc-address "glGetIntegerv"
                        (_fun _ufixint [out : (_ptr o _int)] -> _void
                              -> out)))

(define f (new frame% [label "hello"]))
(define c (new canvas% [parent f]))
(send c with-gl-context
      (λ ()
        (displayln (gl-get-integerv GL-MAJOR-VERSION))
        (displayln (gl-get-integerv GL-MINOR-VERSION))))
