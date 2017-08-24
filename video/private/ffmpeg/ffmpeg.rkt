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

;; This module provides the bindings for the ffmpeg function calls.
;; It is split into submodules for each library associated with ffmpeg,
;;   libavformat, libavutil, etc.
;; Each of these submodules can be required independantly. The top module
;;   provides bindings for all of them.
;; An additional bindings submod provides require/provide glue for the submodules.

(provide (all-defined-out))
(module bindings racket/base
  (require (for-syntax syntax/parse
                       racket/base
                       racket/syntax))
  (define-syntax (reqprov stx)
    (syntax-parse stx
      [(_ mod ...)
       #`(begin
           #,@(for/list ([name (syntax->list #'(mod ...))])
                (cond
                  [(or (identifier? name) (string? (syntax->datum name)))
                   #`(begin (require #,name)
                            (provide (all-from-out #,name)))]
                  [else
                   (syntax-parse name
                     [(for-clause new-name ...)
                      #`(begin
                          #,@(for/list ([name (syntax->list #'(new-name ...))])
                               #`(begin (require (for-clause #,name))
                                        (provide (for-clause (all-from-out #,name))))))])])))]))
  (reqprov racket/match
           racket/set
           racket/dict
           racket/hash
           ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/define/conventions
           setup/dirs
           (for-syntax syntax/parse
                       racket/base
                       racket/syntax)
           "lib.rkt"
           "constants.rkt"
           "data.rkt"))

;; ===================================================================================================

(module avutil racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings))
  
  (define-avutil av-frame-alloc (_fun -> _av-frame-pointer))
  
  (define (av-frame-ref src/dst [maybe-src #f])
    (define-avutil av-frame-ref
      (_fun [out : _av-frame-pointer] _av-frame-pointer -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [else (error "~a : ~a" ret (convert-err ret))])))
    (define src (or maybe-src src/dst))
    (define dst (if maybe-src src/dst (av-frame-alloc)))
    (av-frame-ref dst src))
  (define-avutil av-frame-free (_fun (_ptr i _av-frame-pointer) -> _void))
  (define-avutil av-frame-unref (_fun _av-frame-pointer -> _void))
  (define-avutil av-frame-clone (_fun _av-frame-pointer -> _av-frame-pointer))
  (define-avutil av-frame-make-writable
    (_fun _av-frame-pointer -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (error 'frame-make-writable "~a : ~a" ret (convert-err ret))])))
  (define-avutil av-frame-get-channels (_fun _av-frame-pointer -> _int))
  (define-avutil av-frame-set-channels (_fun _av-frame-pointer _int -> _void))
  (define-avutil av-frame-get-channel-layout (_fun _av-frame-pointer -> _int64))
  (define-avutil av-frame-set-channel-layout (_fun _av-frame-pointer _int64 -> _void))
  (define-avutil av-frame-get-sample-rate (_fun _av-frame-pointer -> _int64))
  (define-avutil av-frame-set-sample-rate (_fun _av-frame-pointer _int64 -> _void))
  (define-avutil av-image-get-buffer-size (_fun _avpixel-format _int _int _int
                                                -> _int))
  (define-avutil av-frame-get-buffer
    (_fun _av-frame-pointer _int -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (error 'av-frame-get-buffer "~a - ~a" ret (convert-err ret))])))
  (define-avutil av-frame-get-colorspace (_fun _av-frame-pointer -> _avcolor-space))
  (define-avutil av-frame-set-colorspace (_fun _av-frame-pointer _avcolor-space -> _void))
  (define-avutil av-frame-get-color-range (_fun _av-frame-pointer -> _avcolor-range))
  (define-avutil av-frame-set-color-range (_fun _av-frame-pointer _avcolor-range -> _void))
  (define-avutil av-frame-get-best-effort-timestamp (_fun _av-frame-pointer -> _int64))
  (define-avutil av-frame-set-best-effort-timestamp (_fun _av-frame-pointer _int -> _void))
  (define-values (av-malloc av-mallocz)
    (let ()
      (define (malloc* proc [a #f] [b #f])
        (define type (cond
                       [(ctype? a) a]
                       [(ctype? b) b]
                       [else _byte]))
        (define size (cond
                       [(integer? a) a]
                       [(integer? b) b]
                       [else 1]))
        (proc (* size (ctype-sizeof type))))
      (define-avutil av-malloc (_fun _size -> _pointer))
      (define-avutil av-mallocz (_fun _size -> _pointer))
      (values (λ ([a #f] [b #f])
                (malloc* av-malloc a b))
              (λ ([a #f] [b #f])
                (malloc* av-mallocz a b)))))
  (define-avutil av-fast-malloc
    (_fun [data : (_ptr io _pointer)] [size : (_ptr io _uint)] _size -> _void
          -> data))
  (define-avutil av-free (_fun _pointer -> _void))
  (define-avutil av-freep (_fun _pointer -> _void))
  (define-avutil av-strdup (_fun _string -> _pointer))
  (define-avutil av-samples-get-buffer-size (_fun _pointer _int _int _avsample-format _int
                                                  -> [ret : _int]
                                                  -> (let ()
                                                       (when (< ret 0)
                                                         (error "sample error"))
                                                       ret)))
  (define-avutil av-opt-set-int (_fun _pointer _string _int64 _av-opt-search-flags
                                      -> [ret : _int]
                                      -> (when (< ret 0)
                                           (error "AV_OPT"))))
  (define-avutil av-opt-set-q (_fun _pointer _string _int64 _av-opt-search-flags
                                    -> [ret : _int]
                                    -> (when (< ret 0)
                                         (error "AV_OPT"))))
  (define-avutil av-opt-set (_fun _pointer _string _string _av-opt-search-flags
                                  -> [ret : _int]
                                  -> (when (< ret 0)
                                       (error "AV_OPT"))))
  (define-avutil av-opt-set-bin (_fun _pointer _string _pointer _int _av-opt-search-flags
                                      -> [ret : _int]
                                      -> (when (< ret 0)
                                           (error "AV_OPT"))))
  (define-avutil av-opt-set-image-size (_fun _pointer _string _int _int _av-opt-search-flags
                                             -> [ret : _int]
                                             -> (when (< ret 0)
                                                  (error "AV_OPT"))))
  (define-avutil av-opt-set-pixel-fmt (_fun _pointer _string _avpixel-format _av-opt-search-flags
                                            -> [ret : _int]
                                            -> (when (< ret 0)
                                                 (error "AV_OPT"))))
  (define-avutil av-opt-set-sample-fmt (_fun _pointer _string _avsample-format _av-opt-search-flags
                                             -> [ret : _int]
                                             -> (when (< ret 0)
                                                  (error "AV_OPT"))))
  (define-avutil av-opt-set-video-rate (_fun _pointer _string _avrational _av-opt-search-flags
                                             -> [ret : _int]
                                             -> (when (< ret 0)
                                                  (error "AV_OPT"))))
  (define-avutil av-opt-set-channel-layout
    (_fun _pointer _string _av-channel-layout _av-opt-search-flags -> [ret : _int]
          -> (when (< ret 0)
               (error "AV_OPT"))))
  (define-avutil av-opt-find
    (_fun _pointer _string _string _av-opt-flags _av-opt-search-flags -> _av-option-pointer/null))
  (define (av-dict-set pm/key key/val val/flags [maybe-flags (void)])
    (define-avutil av-dict-set
      (_fun [out : (_ptr io _av-dictionary-pointer/null)] _string _string _av-dictionary-flags
            -> [ret : _int]
            -> (cond
                 [(>= ret 0) out]
                 [else (error 'av-dict-set (convert-err ret))])))
    (define flags (if (void? maybe-flags) val/flags maybe-flags))
    (define val (if (void? maybe-flags) key/val val/flags))
    (define key (if (void? maybe-flags) pm/key key/val))
    (define pm (if (void? maybe-flags) #f pm/key))
    (av-dict-set pm key val flags))
  (define-avutil av-dict-free (_fun (_ptr i _av-dictionary-pointer/null) -> _void))
  (define-avutil av-dict-count (_fun _av-dictionary-pointer -> _int))
  (define-avutil av-dict-get
    (_fun _av-dictionary-pointer _string _av-dictionary-entry-pointer _av-dictionary-flags
          -> _av-dictionary-entry-pointer))
  (define (av-dict-copy dst/src src/flags [flags #f])
    (define-avutil av-dict-copy (_fun [out : (_ptr io _av-dictionary-pointer/null)]
                                      _av-dictionary-pointer/null
                                      _av-dictionary-flags
                                      -> _void
                                      -> out))
    (define dst (and flags dst/src))
    (define src (if dst src/flags dst/src))
    (define flgs (or flags src/flags))
    (av-dict-copy dst src flgs))
  (define-avutil av-get-channel-layout-nb-channels (_fun _av-channel-layout -> _int))
  (define-avutil av-compare-ts (_fun _int64 _avrational _int64 _avrational
                                     -> _int))
  (define-avutil av-gettime (_fun -> _int64))
  (define-avutil av-gettime-relative (_fun -> _int64))
  (define-avutil av-gettime-relative-is-monotonic (_fun -> _bool))
  (define-avutil av-usleep (_fun _uint -> [ret : _int]
                                 -> (cond
                                      [(= ret 0) (void)]
                                      [else (error 'usleep "~a : ~a" ret (convert-err ret))])))
  (define-avutil av-image-fill-arrays (_fun (_array _pointer 4)
                                            (_array _int 4)
                                            _pointer ;; XXX FIXME
                                            _avpixel-format
                                            _int
                                            _int
                                            _int
                                            -> [ret : _int]
                                            -> (let ()
                                                 (when (< ret 0)
                                                   (error "av-image"))
                                                 ret)))
  (define-avutil av-log-set-level (_fun -> _int))
  (define-avutil av-log-get-level (_fun -> _int))
  (define-avutil av-log-set-flags (_fun _av-log-flags -> _void))
  (define-avutil av-log-get-flags (_fun -> _av-log-flags))
  (define av-log
    (let ()
      (define interfaces (make-hash))
      (lambda (avcl level str . args)
        (define itypes
          (list* _pointer _int _string
                 (for/list ([x (in-list args)])
                   (cond [(and (integer? x) (exact? x)) _int]
                         [(real? x) _double*]
                         [(string? x) _string]
                         [(bytes? x) _bytes]
                         [(symbol? x) _symbol]
                         [(boolean? x) _bool]
                         [else (error 'av-log "Invalid variable type in ~a" x)]))))
        (define av-log (hash-ref! interfaces itypes
                                  (λ ()
                                    (get-ffi-obj "av_log" avutil-lib 
                                                 (_cprocedure itypes _void)))))
        (apply av-log avcl level str args))))
  (define-avutil av-vlog (_fun _pointer _int _string _pointer -> _void))
  (define-avutil av-log-set-callback (_fun _fpointer -> _void))
  ;(_fun _pointer _int _string _pointer -> _void) -> _void))
  ;; XXX va_list is NOT a pointer size. Need to extend libffi before we can use these.
  ;(define-avutil av-log-default-callback (_fun _pointer _int _string _pointer -> _void))
  (define-avutil av-log-format-line2
    (_fun _pointer _int _string _pointer [out : (_bytes o len)] [len : _int] _intptr
          -> [ret : _int]
          -> (cond
               [(< ret 0) (error 'av-log-format-line2 "~a : ~a" ret (convert-err ret))]
               [else
                (define out-len (min (sub1 len) ret))
                (bytes->string/locale (subbytes out 0 out-len))]))))
(require 'avutil)
(provide (all-from-out 'avutil))
  
;; ===================================================================================================

(module avcodec racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings)
           (submod ".." avutil))
  (define (av-packet-ref dst/src [src #f])
    (define-avcodec av-packet-ref (_fun [out : _avpacket-pointer]
                                        _avpacket-pointer
                                        -> [ret : _int]
                                        -> (cond
                                             [(< ret 0)
                                              (unless src
                                                (av-packet-unref out))
                                              #f]
                                             [else out])))
    (if src
        (av-packet-ref dst/src src)
        (av-packet-ref (av-packet-alloc) dst/src)))
  
  (define-avcodec av-packet-unref (_fun _avpacket-pointer
                                        -> _void))
  (define-avcodec av-packet-alloc (_fun -> _avpacket-pointer))
  (define-avcodec av-packet-free (_fun (_ptr io _avpacket-pointer/null) -> _void))
  (define-avcodec av-new-packet (_fun _avpacket-pointer _size -> [ret : _int]
                                      -> (cond
                                           [(= ret 0) (void)]
                                           [else (error "~a : ~a" ret (convert-err ret))])))
  (define-avcodec av-init-packet (_fun _avpacket-pointer -> _void))
  (define-avcodec av-dup-packet (_fun _avpacket-pointer
                                      -> [ret : _int]
                                      -> (unless (= ret 0)
                                           (error "dup-packet?"))))
  (define-avcodec av-packet-rescale-ts (_fun _avpacket-pointer _avrational _avrational
                                             -> _void))
  (define-avcodec avcodec-find-encoder (_fun _avcodec-id
                                             -> _avcodec-pointer))
  (define-avcodec avcodec-find-decoder (_fun _avcodec-id
                                             -> _avcodec-pointer))
  (define-avcodec avcodec-alloc-context3 (_fun _avcodec-pointer/null
                                               -> _avcodec-context-pointer))
  (define-avcodec avcodec-parameters-alloc (_fun -> _avcodec-parameters-pointer))
  (define-avcodec avcodec-parameters-free (_fun (_ptr i _avcodec-parameters-pointer) -> _void))
  (define (avcodec-parameters-from-context param/context [context #f])
    (define-avcodec avcodec-parameters-from-context
      (_fun [out : _avcodec-parameters-pointer] _avcodec-context-pointer
            -> [ret : _int]
            -> (cond
                 [(>= ret 0) out]
                 [else (error 'avcoded-parameters-from-context (convert-err ret))])))
    (define param
      (if context
          param/context
          (avcodec-parameters-alloc)))
    (define context*
      (or context param/context))
    (avcodec-parameters-from-context param context*))
  (define (avcodec-parameters-to-context codec/ctx/param [maybe-param #f])
    (define param (or maybe-param codec/ctx/param))
    (define ctx
      (cond
        [(avcodec-context? codec/ctx/param) codec/ctx/param]
        [(avcodec? codec/ctx/param) (avcodec-alloc-context3 codec/ctx/param)]
        [else (avcodec-alloc-context3 (avcodec-find-decoder (avcodec-parameters-codec-id param)))]))
    (define-avcodec avcodec-parameters-to-context
      (_fun [out : _avcodec-context-pointer] _avcodec-parameters-pointer
            -> [ret : _int]
            -> (cond
                 [(>= ret 0) out]
                 [else (error 'parameters-to-context (convert-err ret))])))
    (avcodec-parameters-to-context ctx param))
  (define (avcodec-open2 ctx codec [dict #f])
    (define-avcodec avcodec-open2
      (_fun _avcodec-context-pointer
            _avcodec-pointer
            [dict : _pointer];(_ptr io _av-dictionary-pointer/null)]
            -> [ret : _int]
            -> (cond [(= ret 0) dict]
                     [(= (- ret) EINVAL) (error 'avcodec-open2 "Invalid Argument")]
                     [else (error 'vcodec-open2 (format "~a, ~a" ret (convert-err ret)))])))
    (avcodec-open2 ctx codec dict))
  (define-avcodec avcodec-close (_fun _avcodec-context-pointer/null -> _int))
  (define-avcodec avcodec-send-packet
    (_fun _avcodec-context-pointer _avpacket-pointer/null
          -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [(= (- ret) EAGAIN)
                (raise (exn:ffmpeg:again "send-packet" (current-continuation-marks)))]
               [(= ret AVERROR-EOF)
                (raise (exn:ffmpeg:eof "send-packet" (current-continuation-marks)))]
               [else (error 'send-packet "ERROR (~a): ~a" ret (convert-err ret))])))
  (define (avcodec-receive-packet ctx [maybe-packet #f])
    (define-avcodec avcodec-receive-packet
      (_fun _avcodec-context-pointer
            [out : _avpacket-pointer]
            -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [else
                  (unless maybe-packet
                    (av-packet-free out))
                  (cond
                    [(= (- ret) EAGAIN)
                     (raise (exn:ffmpeg:again "receive-packet/again" (current-continuation-marks)))]
                    [(= ret AVERROR-EOF)
                     (raise (exn:ffmpeg:eof "receive-packet/eof" (current-continuation-marks)))]
                    [else
                     (error 'recev-packet (convert-err ret))])])))
    (define packet (or maybe-packet (av-packet-alloc)))
    (avcodec-receive-packet ctx packet))
  (define-avcodec avcodec-send-frame
    (_fun _avcodec-context-pointer _av-frame-pointer/null
          -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [(= (- ret) EAGAIN)
                (raise (exn:ffmpeg:again "send-frame" (current-continuation-marks)))]
               [(= ret AVERROR-EOF)
                (raise (exn:ffmpeg:eof "send-frame" (current-continuation-marks)))]
               [else
                (error 'send-frame "ERROR: ~a : ~a" ret (convert-err ret))])))
  (define (avcodec-receive-frame ctxt [maybe-frame #f])
    (define-avcodec avcodec-receive-frame
      (_fun _avcodec-context-pointer
            [out : _av-frame-pointer]
            -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [else
                  (unless maybe-frame
                    (av-frame-free out))
                  (cond
                    [(= (- ret) EAGAIN)
                     (raise (exn:ffmpeg:again "receive-frame: eagain" (current-continuation-marks)))]
                    [(= ret AVERROR-EOF)
                     (raise (exn:ffmpeg:eof "receive-frame: eof" (current-continuation-marks)))]
                    [else
                     (error 'recev-frame "Error: ~a" (convert-err ret))])])))
    (define frame (or maybe-frame (av-frame-alloc)))
    (avcodec-receive-frame ctxt frame))
  (define-avcodec avcodec-flush-buffers (_fun _avcodec-context-pointer -> _void)))

(require 'avcodec)
(provide (all-from-out 'avcodec))

;; ===================================================================================================

(module avformat racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings)
           (submod ".." avcodec))
  
  (define-avformat av-register-all (_fun -> _void))
  
  (define-avformat avformat-network-init (_fun -> _int -> (void)))

  (define (avformat-open-input ctx/path path/input input/opt [opt (void)])
    (define-avformat avformat-open-input
      (_fun (out : (_ptr io _avformat-context-pointer/null))
            _path
            _av-input-format-pointer/null
            (_ptr io _av-dictionary-pointer/null)
            -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [(< ret 0)
                  (error 'avformat "~a : ~a" ret (convert-err ret))])))
    (define opt* (if (void? opt) input/opt opt))
    (define input (if (void? opt) path/input input/opt))
    (define path (if (void? opt) ctx/path path/input))
    (define ctx (if (void? opt) #f ctx/path))
    (avformat-open-input ctx path input opt*))

  (define-avformat avformat-find-stream-info (_fun _avformat-context-pointer
                                                   _pointer
                                                   -> [r : _int]
                                                   -> (let ()
                                                        (when (< r 0) (error "NOO2"))
                                                        (void))))
  
  (define-avformat avformat-close-input (_fun (_ptr i _avformat-context-pointer)
                                              -> _void))

  (define-avformat avformat-alloc-context (_fun -> _avformat-context-pointer))
  
  (define-avformat avformat-free-context (_fun _avformat-context-pointer -> _void))
  
  (define-avformat av-dump-format (_fun _avformat-context-pointer _int _path _is-output
                                        -> _void))
  
  (define (av-read-frame ctx [frame #f])
    (define-avformat av-read-frame (_fun _avformat-context-pointer
                                         [out : _avpacket-pointer]
                                         -> [ret : _int]
                                         -> (cond
                                              [(< ret 0)
                                               (unless frame
                                                 (av-packet-unref out))
                                               #f]
                                              [else out])))
    (define frame* (or frame (av-packet-alloc)))
    (av-read-frame ctx frame*))
  
  (define-avformat av-seek-frame
    (_fun _avformat-context-pointer [idx : _int] [ts : _int64] _avseek-flags -> [ret : _int]
          -> (when (< ret 0)
               (error 'av-seek-frame "IDX:~a TS:~a : ~a : ~a" idx ts ret (convert-err ret)))))
  
  ; XXX This is not yet part of the stable API (per the ffmpeg docs)
  ;(define-avformat avformat-seek-file
  ;  (_fun _avformat-context-pointer _int _int64 _int64 _int64 _int -> [ret : _int]
  ;        -> (when (< ret 0)
  ;             (error 'avformat-seek-file "~a : ~a" ret (convert-err ret)))))
  (define-avformat avformat-flush
    (_fun _avformat-context-pointer -> [ret : _int]
          -> (when (< ret 0)
               (error 'avformat-flush "~a : ~a" ret (convert-err ret)))))
  
  (define-avformat av-read-play (_fun _avformat-context-pointer -> [ret : _int]))
  (define-avformat av-read-pause (_fun _avformat-context-pointer -> [ret : _int]))

  
  (define-avformat av-guess-format (_fun _string _string _string -> _av-output-format-pointer))
  
  (define-avformat av-guess-frame-rate
    (_fun _avformat-context-pointer _avstream-pointer _av-frame-pointer/null -> [ret : _avrational]
          -> (cond [(and (= (numerator ret) 0)
                         (= (denominator ret) 1))
                    (error 'av-guess-framerate "Cannot guess framerate")]
                   [else ret])))
  
  (define-avformat av-guess-sample-aspect-ratio
    (_fun _avformat-context-pointer _avstream-pointer _av-frame-pointer/null -> [ret : _avrational]
          -> (cond [(and (= (numerator ret) 0)
                         (= (denominator ret) 1))
                    (error 'av-guess-sample-aspect-ratio "Cannot guess sample aspect ratio")]
                   [else ret])))
  
  (define-avformat av-guess-codec
    (_fun _av-output-format-pointer _string _string _string _avmedia-type -> _avcodec-id))
  
  (define-avformat avformat-alloc-output-context2
    (_fun [out : (_ptr o _avformat-context-pointer)] _av-output-format-pointer/null _string _string
          -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'alloc-ouput-context2 (convert-err ret))])))
  
  (define-avformat avformat-new-stream (_fun _avformat-context-pointer _avcodec-pointer/null
                                             -> _avstream-pointer))
  
  (define-avformat avformat-write-header
    (_fun _avformat-context-pointer _pointer ;(_ptr io _av-dictionary-pointer/null)
          -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (convert-err ret)])))
  
  (define-avformat av-write-trailer (_fun _avformat-context-pointer -> [ret : _int]
                                          -> (cond
                                               [(= ret 0) (void)]
                                               [else (convert-err ret)])))
  
  (define-avformat av-write-frame (_fun _avformat-context-pointer _avpacket-pointer/null
                                        -> [ret : _int]
                                        -> (cond
                                             [(= ret 0) (void)]
                                             [(< ret 0) (error 'write-frame (convert-err ret))]
                                             [(> ret 0)
                                              (raise (exn:ffmpeg:flush
                                                      "send-frame"
                                                      (current-continuation-marks)))])))
  
  (define-avformat av-interleaved-write-frame (_fun _avformat-context-pointer _avpacket-pointer/null
                                                    -> [ret : _int]
                                                    -> (cond
                                                         [(= ret 0) (void)]
                                                         [(< ret 0)
                                                          (error 'write-frame (convert-err ret))]
                                                         [(> ret 0)
                                                          (raise (exn:ffmpeg:flush
                                                                  "send-frame"
                                                                  (current-continuation-marks)))])))

  (define-avformat avio-alloc-context
    (_fun _pointer _int _bool _pointer
          (_fun _pointer _pointer _int -> _int)
          (_fun _pointer _pointer _int -> _int)
          (_fun _pointer _int64 _int -> _int64)
          -> _avio-context-pointer))
  
  (define-avformat avio-open
    (_fun [out : (_ptr io _avio-context-pointer/null)] [p : _path] _avio-flags
          -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'avio-open "path ~a: ~a : ~a" p ret (convert-err ret))])))
  
  (define-avformat avio-close (_fun _avio-context-pointer -> [ret : _int]
                                    -> (cond
                                         [(= ret 0) (void)]
                                         [else (error 'avio-close (convert-err ret))])))

  (define-avformat av-find-input-format
    (_fun _string -> _av-input-format-pointer))
  
  (define-avformat av-find-best-stream
    (_fun _avformat-context-pointer _avmedia-type _int _int
          _pointer ;(_ptr io _avcodec-pointer/null)
          _int
          -> [ret : _int]
          -> (cond [(>= ret 0) ret]
                   [(= (- ret) AVERROR-STREAM-NOT-FOUND (FFERRTAG #xf8 #\S #\T #\R))
                    (raise (exn:ffmpeg:stream-not-found
                            "find-best-frame"
                            (current-continuation-marks)))]
                   [(= (- ret) AVERROR-DECODER-NOT-FOUND (FFERRTAG #xf8 #\D #\E #\C))
                    (raise (exn:ffmpeg:decoder-not-found
                            "find-best-stream"
                            (current-continuation-marks)))]
                   [else (raise (convert-err ret))])))
  
  (define-avformat av-new-program (_fun _avformat-context-pointer _int -> _av-program-pointer))
  
  (define-avformat av-stream-get-end-pts (_fun _avstream-pointer -> _int64))
  
  (define-avformat av-stream-set-r-frame-rate (_fun _avstream-pointer _avrational -> _void))
  
  (define-avformat av-stream-get-r-frame-rate (_fun _avstream-pointer -> _avrational)))

(require 'avformat)
(provide (all-from-out 'avformat))

;; ===================================================================================================

(module swscale racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings))
  (define-swscale sws-getContext (_fun _int
                                       _int
                                       _avpixel-format
                                       _int
                                       _int
                                       _avpixel-format
                                       _sws-flags
                                       _pointer
                                       _pointer
                                       _pointer
                                       -> _sws-context-pointer))
  (define-swscale sws-scale (_fun _sws-context-pointer
                                  _pointer
                                  _pointer
                                  _int
                                  _int
                                  _pointer
                                  _pointer
                                  -> _int)))

(require 'swscale)
(provide (all-from-out 'swscale))

;; ===================================================================================================

(module swresample racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings))
  (define-swresample swr-alloc (_fun -> _swr-context-pointer))
  (define-swresample swr-init (_fun _swr-context-pointer -> [ret : _int]
                                    -> (when (< ret 0)
                                         (error "SWR"))))
  (define-swresample swr-convert (_fun _swr-context-pointer
                                       _pointer
                                       _int
                                       _pointer
                                       _int
                                       -> _int)))

(require 'swresample)
(provide (all-from-out 'swresample))
  
;; ===================================================================================================

(module avfilter racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings)
           (submod ".." avutil))
  
  (define-avfilter avfilter-register-all (_fun -> _void))
  (define-avfilter avfilter-graph-alloc (_fun -> _avfilter-graph-pointer))
  (define-avfilter avfilter-graph-free (_fun (_ptr io _avfilter-graph-pointer/null) -> _void))
  (define-avfilter avfilter-graph-alloc-filter
    (_fun _avfilter-graph-pointer _avfilter-pointer _string -> _avfilter-context-pointer))
  (define-avfilter avfilter-graph-get-filter
    (_fun _avfilter-graph-pointer _string -> _avfilter-context-pointer/null))
  (define-avfilter avfilter-graph-create-filter
    (_fun [out : (_ptr o _avfilter-context-pointer/null)]
          _avfilter-pointer
          _string
          _string
          _pointer
          _avfilter-graph-pointer
          -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'graph-create-filter "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-graph-parse
    (_fun _avfilter-graph-pointer
          _string
          _avfilter-in-out-pointer/null
          _avfilter-in-out-pointer/null
          _pointer
          -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (error 'graph-parse "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-graph-parse-ptr
    (_fun _avfilter-graph-pointer
          _string
          [in : (_ptr io _avfilter-in-out-pointer/null)]
          [out : (_ptr io _avfilter-in-out-pointer/null)]
          _pointer
          -> [ret : _int]
          -> (cond
               [(>= ret 0) (values in out)]
               [else (error 'graph-parse-ptr "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-graph-parse2
    (_fun _avfilter-graph-pointer
          _string
          [in : (_ptr io _avfilter-in-out-pointer/null)]
          [out : (_ptr io _avfilter-in-out-pointer/null)]
          -> [ret : _int]
          -> (cond
               [(= ret 0) (values in out)]
               [else (error 'graph-parse-ptr "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-graph-config
    (_fun _avfilter-graph-pointer _pointer -> [ret : _int]
          -> (cond
               [(>= ret 0) (void)]
               [else (error 'graph-config "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-graph-set-auto-convert
    (_fun _avfilter-graph-pointer _avfilter-auto-convert -> _void))
  (define-avfilter avfilter-get-by-name (_fun _string -> [ret : _avfilter-pointer/null]
                                              -> (or ret (error 'avfilter "Invalid Filter Name"))))
  (define-avfilter avfilter-inout-alloc (_fun -> _avfilter-in-out-pointer))
  (define-avfilter avfilter-inout-free (_fun (_ptr i _avfilter-in-out-pointer/null) -> _void))
  (define (av-buffersink-get-frame ptr [out #f])
    (define-avfilter av-buffersink-get-frame
      (_fun _avfilter-context-pointer [out* : _av-frame-pointer]
            -> [ret : _int]
            -> (cond
                 [(>= ret 0) out*]
                 [else
                  (unless out
                    (av-frame-free out*))
                  (cond
                    [(= (- ret) EAGAIN)
                     (raise (exn:ffmpeg:again "buffersink-get-frame/again"
                                              (current-continuation-marks)))]
                    [(= ret AVERROR-EOF)
                     (raise (exn:ffmpeg:eof "buffersink-get-frame/eof" (current-continuation-marks)))]
                    [else (error 'buffersink-get-frame/other (convert-err ret))])])))
    (define o (or out (av-frame-alloc)))
    (av-buffersink-get-frame ptr o))
  (define (av-buffersink-get-frame-flags ptr flags/frame [maybe-flags #f])
    (define-avfilter av-buffersink-get-frame-flags
      (_fun _avfilter-context-pointer [out : _av-frame-pointer] _av-buffer-sink-flags -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [else
                  (unless maybe-flags
                    (av-frame-free out))
                  (cond
                    [(= (- ret) EAGAIN)
                     (raise (exn:ffmpeg:again "buffersink-get-frame-flags"
                                              (current-continuation-marks)))]
                    [(= ret AVERROR-EOF)
                     (raise (exn:ffmpeg:eof "buffersink-get-frame" (current-continuation-marks)))]
                    [else (error 'buffersink-get-frame-flags (convert-err ret))])])))
    (define flags (or maybe-flags flags/frame))
    (define frame (if maybe-flags
                      flags/frame
                      (av-frame-alloc)))
    (av-buffersink-get-frame-flags ptr frame flags))
  (define (av-buffersink-get-samples ptr frame/nb-samples [maybe-samples #f])
    (define-avfilter av-buffersink-get-samples
      (_fun _avfilter-context-pointer [out : _av-frame-pointer] _int
            -> [ret : _int]
            -> (cond
                 [(= ret 0) out]
                 [else
                  (unless maybe-samples
                    (av-frame-free out))
                  (cond
                    [(= (- ret) EAGAIN)
                     (raise (exn:ffmpeg:again "buffersink-get-frame-flags"
                                              (current-continuation-marks)))]
                    [(= ret AVERROR-EOF)
                     (raise (exn:ffmpeg:eof "buffersink-get-frame" (current-continuation-marks)))]
                    [else (error 'buffersink-get-frame-flags (convert-err ret))])])))
    (define samples (or maybe-samples frame/nb-samples))
    (define frame (if maybe-samples
                      frame/nb-samples
                      (av-frame-alloc)))
    (av-buffersink-get-samples ptr frame samples))

  ;; The following functions were defined in ffmpeg 3.3, but we don't
  ;;    *need* them if they are not available.
  ;; We keep them around because they are going to be slightly more accurate
  ;;    than pre-computing the result.
  ;; The make-fail is applied to the these defines rather than the define-ff-definer
  ;;    because we want to check for the other functions eagerly.
  (define-avfilter av-buffersink-get-type (_fun _avfilter-context-pointer -> _avmedia-type)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-time-base (_fun _avfilter-context-pointer -> _avrational)
    #:make-fail make-not-available)
  (define (av-buffersink-get-format ptr [type #f])
    (define-avfilter av-buffersink-get-format (_fun _avfilter-context-pointer -> _int)
      #:make-fail make-not-available)
    (define ret (av-buffersink-get-format ptr))
    (define type* (or type (av-buffersink-get-type)))
    (match type*
      ['video (cast ret _int _avpixel-format)]
      ['audio (cast ret _int _avsample-format)]
      ['raw ret]))
  (define-avfilter av-buffersink-get-frame-rate (_fun _avfilter-context-pointer -> _avrational)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-w (_fun _avfilter-context-pointer -> _int)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-h (_fun _avfilter-context-pointer -> _int)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-sample-aspect-ratio
    (_fun _avfilter-context-pointer -> _avrational)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-channels
    (_fun _avfilter-context-pointer -> _int)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-channel-layout
    (_fun _avfilter-context-pointer -> _av-channel-layout)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-sample-rate
    (_fun _avfilter-context-pointer -> _int)
    #:make-fail make-not-available)
  (define-avfilter av-buffersink-get-hw-frames-ctx
    (_fun _avfilter-context-pointer -> _avbuffer-ref-pointer)
    #:make-fail make-not-available)
  
  (define-avfilter av-buffersink-set-frame-size (_fun _avfilter-context-pointer _uint -> _void))
  (define-avfilter av-buffersink-params-alloc (_fun -> _av-buffersink-params-pointer))
  (define-avfilter av-abuffersink-params-alloc (_fun -> _av-buffersink-aparams-pointer))
  (define-avfilter av-buffersrc-parameters-alloc (_fun -> _av-buffersrc-parameters-pointer))
  (define-avfilter av-buffersrc-add-frame-flags
    (_fun _avfilter-context-pointer _av-frame-pointer/null _av-buffer-src-flags -> [ret : _int]
          -> (cond
               [(>= ret 0) (void)]
               [else (error 'buffersrc-add-frame-flags "~a : ~a" ret (convert-err ret))])))
  (define-avfilter av-buffersrc-add-frame
    (_fun _avfilter-context-pointer _av-frame-pointer/null -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (error 'buffersrc-add-frame "~a : ~a" ret (convert-err ret))])))
  (define-avfilter av-buffersrc-write-frame
    (_fun _avfilter-context-pointer _av-frame-pointer/null -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else (error 'buffersrc-add-frame "~a : ~a" ret (convert-err ret))])))
  (define-avfilter av-buffersrc-get-nb-failed-requests (_fun _avfilter-context-pointer -> _int))
  (define-avfilter avfilter-init-str
    (_fun _avfilter-context-pointer _string -> [ret : _int]
          -> (cond
               [(= ret 0) (void)]
               [else
                (error 'avfilter-init-str "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-init-dict
    (_fun _avfilter-context-pointer _pointer -> [ret : _int]
          -> (cond
               [(= ret 0) void]
               [else
                (error 'avfilter-init-str "~a : ~a" ret (convert-err ret))])))
  (define-avfilter avfilter-free (_fun _avfilter-context-pointer -> _void))
  (define-avfilter avfilter-pad-count (_fun _avfilter-pad-pointer -> _int))
  (define-avfilter avfilter-pad-get-name (_fun _avfilter-pad-pointer _int -> _string))
  (define-avfilter avfilter-pad-get-type (_fun _avfilter-pad-pointer _int -> _avmedia-type)))

(require 'avfilter)
(provide (all-from-out 'avfilter))

;; ===================================================================================================

(module avdevice racket/base
  (provide (all-defined-out))
  (require (submod ".." bindings))

  (define-avdevice avdevice-register-all (_fun -> _void))

  (define-avdevice av-input-audio-device-next
    (_fun _av-input-format-pointer/null -> _av-input-format-pointer/null))
  (define-avdevice av-input-video-device-next
    (_fun _av-input-format-pointer/null -> _av-input-format-pointer/null))
  (define-avdevice av-output-audio-device-next
    (_fun _av-output-format-pointer/null -> _av-output-format-pointer/null))
  (define-avdevice av-output-video-device-next
    (_fun _av-output-format-pointer/null -> _av-output-format-pointer/null))

  (define-avdevice avdevice-app-to-dev-control-message
    (_fun _avformat-context-pointer _av-app->dev-message-type _pointer _size
          -> [ret : _int]
          -> (cond [(>= ret 0) (void)]
                   [else
                    (error 'avdevice-app-to-dev-control-message "~a : ~a" ret (convert-err ret))])))
  (define-avdevice avdevice-dev-to-app-control-message
    (_fun _avformat-context-pointer _av-dev->app-message-type _pointer _size
          -> [ret : _int]
          -> (cond [(>= ret 0) (void)]
                   [else
                    (error 'avdevice-dev-to-app-control-message "~a : ~a" ret (convert-err ret))])))

  (define-avdevice avdevice-capabilities-create
    (_fun [out : (_ptr o _avdevice-capabilities-query-pointer)] _avformat-context-pointer _pointer
          -> [ret : _int]
          -> (cond [(>= ret 0) out]
                   [else (error 'avdevice-capabilities-create "~a : ~a" ret (convert-err ret))])))
  (define-avdevice avdevice-capabilities-free
    (_fun (_ptr i _avdevice-capabilities-query-pointer/null) _avformat-context-pointer -> _void))
  
  (define-avdevice avdevice-list-devices
    (_fun _avformat-context-pointer [out : (_ptr o _avdevice-info-list-pointer/null)] -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'avdevice-list-devices "~a : ~a" ret (convert-err ret))])))
  (define-avdevice avdevice-free-list-devices
    (_fun (_ptr i _avdevice-info-list-pointer/null) -> _void))
  (define-avdevice avdevice-list-input-sources
    (_fun _av-input-format-pointer
          _string
          _av-dictionary-pointer/null
          [out : (_ptr o _avdevice-info-list-pointer/null)]
          -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'avdevice-list-input-sources "~a : ~a" ret (convert-err ret))])))
  (define-avdevice avdevice-list-output-sinks
    (_fun _av-input-format-pointer
          _string
          _av-dictionary-pointer/null
          [out : (_ptr o _avdevice-info-list-pointer/null)]
          -> [ret : _int]
          -> (cond
               [(>= ret 0) out]
               [else (error 'avdevice-list-output-sinks "~a : ~a" ret (convert-err ret))]))))

(require 'avdevice)
(provide (all-from-out 'avdevice))
