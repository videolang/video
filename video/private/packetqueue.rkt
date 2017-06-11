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

(struct exn:packetqueue-empty ())

(struct packetqueue (first     ;; First packet
                    last       ;; Last Packet
                    nb-packets ;; Number of Packets
                    size       ;; Number of frames (some audio packets have multiple frames)
                    mutex      ;; Queue Mutex
                    cond)      ;; Queue Conditional Variable
  #:mutable)

(struct packet-link (packet
                     next)
  #:mutable)

(define (mk-packetqueue)
  (define mutex (mutex-create))
  (define cond-var (cond-create))
  (register-mlt-close mutex-destroy mutex)
  (register-mlt-close cond-destroy cond-var)
  (packetqueue #f #f 0 0 mutex cond-var))
(define (packetqueue-put q p)
  ;(define p (av-packet-ref packet))
  (define p* (packet-link p #f))
  (dynamic-wind
   (λ () (mutex-lock (packetqueue-mutex q)))
   (λ ()
     (cond [(packetqueue-last q)
            (set-packet-link-next! (packetqueue-last q) p*)]
           [else (set-packetqueue-first! q p*)])
     (set-packetqueue-last! q p*)
     (set-packetqueue-nb-packets!
      q (add1 (packetqueue-nb-packets q)))
     (when (avpacket? p)
       (set-packetqueue-size!
        q (+ (packetqueue-nb-packets q)
             (avpacket-size p))))
     (cond-signal (packetqueue-cond q)))
   (λ () (mutex-unlock (packetqueue-mutex q)))))
(define (packetqueue-get q [wait #t])
  (dynamic-wind
   (λ () (mutex-lock (packetqueue-mutex q)))
   (λ ()
     (let loop ()
       (define p (packetqueue-first q))
       (cond [p
              (set-packetqueue-first!
               q (packet-link-next p))
              (set-packetqueue-nb-packets!
               q (sub1 (packetqueue-nb-packets q)))
              (when (avpacket? (packet-link-packet p))
                (set-packetqueue-size!
                 q (- (packetqueue-size q)
                      (avpacket-size (packet-link-packet p)))))
              (packet-link-packet p)]
             [wait (cond-wait (packetqueue-cond q)
                              (packetqueue-mutex q))
                   (loop)]
             [else
              (raise (exn:packetqueue-empty))])))
   (λ () (mutex-unlock (packetqueue-mutex q)))))
