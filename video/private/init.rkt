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

(provide (all-defined-out))
(require racket/async-channel
         racket/match
         racket/struct
         racket/list
         ffi/unsafe
         ffi/unsafe/atomic
         ffi/unsafe/define
         ffi/unsafe/global
         "log.rkt"
         "ffmpeg/libvid.rkt"
         "ffmpeg/main.rkt")

(define video-key #"VIDEO-FFMPEG-INIT")

(define ffmpeg-logger (make-logger 'ffmpeg video-logger))
(define-ffi-definer define-internal #f)

(define ffmpeg-log-list '())
(struct ffmpeg-msg (name
                    level
                    msg))

(define (callback-proc name level len msg)
  (define name* (or name #"???"))
  (set! ffmpeg-log-list (cons (ffmpeg-msg name* level msg) ffmpeg-log-list)))

;; Flush the current log buffer into a list which is returned.
;; The function runs in automic mode to cooperate with the
;;   actual logging callback.
(define (flush-ffmpeg-log-list!)
  (call-as-atomic
   (λ ()
     (define ret ffmpeg-log-list)
     (set! ffmpeg-log-list '())
     (reverse ret))))

;; Flush the ffmpeg log. Unlike `flush-ffmpeg-log-list!`, this
;;    function is not atomic, but does put the log into the actual
;;    racket level logging structure.
;; Returns the number of msgs received and if an
;;   eof character was received.
;; -> (Values Nnnegative-Integer Boolean)
(define (flush-ffmpeg-log!/tracking)
  (define buff (flush-ffmpeg-log-list!))
  (for/fold ([msg-count 0]
             [found-eof? #f])
            ([msg (in-list buff)])
    (match msg
      [(struct* ffmpeg-msg ([name name]
                            [level level]
                            [msg msg*]))
       (define msg (bytes->string/locale msg* #\?))
       (define log-level
         (match level
           ['debug 'debug]
           ['warning 'warning]
           ['error 'error]
           ['info 'info]
           ['fatal 'fatal]
           ['verbose 'info]
           [_ 'info]))
       (when (log-level? ffmpeg-logger log-level)
         (log-message ffmpeg-logger log-level
                      ;'ffmpeg
                      (string->symbol (format "~a" name))
                      (substring msg 0 (sub1 (string-length msg)))
                      (current-continuation-marks)
                      #f))
       (values (add1 msg-count) #f)]
      [x #:when (eof-object? x) (values msg-count #t)])))

;; Like flush-ffmpeg-log!/tracking, but returns void instead.
(define (flush-ffmpeg-log!)
  (flush-ffmpeg-log!/tracking)
  (void))

(define (stop-ffmpeg-logging/not-running)
  (error 'video/init "Not Currently logging"))
(define stop-ffmpeg-logging stop-ffmpeg-logging/not-running)
(define (start-ffmpeg-logging)
  (define stop-flag #f)
  (define t
    (thread
     (λ ()
       (define min-sleep-time 0.1)
       (define max-sleep-time 30)
       (define delta-sleep-time 0.1)
       (let loop ([sleep-time min-sleep-time])
         (define-values (msg-count found-eof?) (flush-ffmpeg-log!/tracking))
         (cond [found-eof? (void)]
               [(= msg-count 0)
                (unless stop-flag
                  (sleep sleep-time)
                  (loop (min (+ sleep-time delta-sleep-time) max-sleep-time)))]
               [else (loop min-sleep-time)])))))
  (set! stop-ffmpeg-logging
    (λ ()
      (set! stop-flag #t)
      (thread-wait t)
      (set! stop-ffmpeg-logging stop-ffmpeg-logging/not-running))))
(start-ffmpeg-logging)

;; Init ffmpeg (ONCE PER PROCESS)
(when (ffmpeg-installed?)
  (unless (register-process-global video-key (cast 1 _racket _pointer))
    (av-register-all)
    (avfilter-register-all)
    (avformat-network-init)
    (avdevice-register-all)))

;; Set up the logger.
;; This must be done a new time the module is instantiated, and
;;    we unset the logger after its finished.
;; Only ONE logger can be installed at a time. This is not a problem
;;  as init should only really be running once.
;; XXX Sadly this is only working on OS X. :(
(define callback-executor (make-will-executor))
(define (finish-execution v)
  (set-racket-log-callback #f))
(will-register callback-executor callback-proc finish-execution)
(when (and (ffmpeg-installed?) (libvid-installed?)
           (eq? (system-type 'os) 'macosx))
  (set-racket-log-callback callback-proc)
  (av-log-set-callback ffmpeg-log-callback)
  (thread
   (λ ()
     (will-execute callback-executor)))
  (plumber-add-flush!
   (current-plumber)
   (λ (handler)
     (stop-ffmpeg-logging)))
  (void))

;; Because portaudio has a nasty tendency to output a lot of garbadge to stdout, only
;; require it in situations where its actually needed.
(module* portaudio racket
  (require ffi/unsafe
           ffi/unsafe/global
           portaudio)
  
  (define portaudio-key #"VIDEO-PORTAUDIO-INIT")
  (unless (register-process-global portaudio-key (cast 1 _racket _pointer))
    (pa-maybe-initialize)))
