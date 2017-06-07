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

(require "threading.rkt"
         "init-mlt.rkt"
         "ffmpeg.rkt")

(struct exn:audioqueue-empty ())

(struct audioqueue (first
                    last
                    nb-packets
                    size
                    mutex
                    cond)
  #:mutable)

(struct packet-link (packet
                     next)
  #:mutable)

(define (mk-audioqueue)
  (define mutex (mutex-create))
  (define cond-var (cond-create))
  (register-mlt-close mutex-destroy mutex)
  (register-mlt-close cond-destroy cond-var)
  (audioqueue #f #f 0 0 mutex cond-var))
(define (audioqueue-put q p)
  ;(define p (av-packet-ref packet))
  (define p* (packet-link p #f))
  (dynamic-wind
   (λ () (mutex-lock (audioqueue-mutex q)))
   (λ ()
     (cond [(audioqueue-last q)
            (set-packet-link-next! (audioqueue-last q) p*)]
           [else (set-audioqueue-first! q p*)])
     (set-audioqueue-last! q p*)
     (set-audioqueue-nb-packets!
      q (add1 (audioqueue-nb-packets q)))
     (set-audioqueue-size!
      q (+ (audioqueue-nb-packets q)
           (avpacket-size p)))
     (cond-signal (audioqueue-cond q)))
   (λ () (mutex-unlock (audioqueue-mutex q)))))
(define (audioqueue-get q [wait #t])
  (dynamic-wind
   (λ () (mutex-lock (audioqueue-mutex q)))
   (λ ()
     (let loop ()
       (define p (audioqueue-first q))
       (cond [p
              (set-audioqueue-first!
               q (packet-link-next p))
              (set-audioqueue-nb-packets!
               q (sub1 (audioqueue-nb-packets q)))
              (set-audioqueue-size!
               q (- (audioqueue-size q)
                    (avpacket-size (packet-link-packet p))))
              (packet-link-packet p)]
             [wait (cond-wait (audioqueue-cond q)
                              (audioqueue-mutex q))
                   (loop)]
             [else
              (raise (exn:audioqueue-empty))])))
   (λ () (mutex-unlock (audioqueue-mutex q)))))
