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
(require pict
         ppict/pict
         ppict/tag
         racket/math
         scribble/manual
         racket/list)

(define (playlist-timeline #:distance [distance 5]
                                #:start [start #f]
                                #:end [end #f]
                                . trace)
  (define frames
   (apply hc-append distance trace))
  (vc-append
   15
   frames
   (let ([p (hc-append (pict-width frames)
                       (tag-pict (if start (vline 1 10) (blank)) 'start)
                       (tag-pict (if end (vline 1 10) (blank)) 'end))])
     (pin-arrow-line 5 p #:label (text "time")
                     (find-tag p 'start) cc-find
                     (find-tag p 'end) cc-find))))

(define (ellipses #:offset [offset 3]
                  #:size [size 2])
  (hc-append
   offset
   (disk size)
   (disk size)
   (disk size)))

(define (scale-1080p p [w-size 60] [mode 'inset])
  (scale-to-fit p w-size (* 9/16 w-size)
                #:mode mode))

(define (shot p)
  (clip
   (frame
    (scale-1080p p))))

(define (rotating-rect-clip n)
    (build-list n
                (λ (f#)
                  (shot (rotate (filled-rectangle 15 15 #:color "red")
                                (/ (* f# pi) n 2))))))

(define the-rr-clip (rotating-rect-clip 10))

(define (ball-drop-clip n)
  (build-list n
              (λ (f#)
                (shot (ppict-do (blank 50)
                                #:go (coord 1/2 (/ f# n) 'cc)
                                (disk 25 #:color "green"))))))

(define the-ball-drop (ball-drop-clip 10))

(define (inset-flow n)
  (nested #:style 'inset n))

(define (grayscale-pict s)
  (define buf (pict->argb-pixels s))
  (define gray-buf
    (apply bytes
           (append*
            (for/list ([i (in-range 0 (bytes-length buf) 4)])
              (define a (bytes-ref buf i))
              (define r (bytes-ref buf (+ i 1)))
              (define g (bytes-ref buf (+ i 2)))
              (define b (bytes-ref buf (+ i 3)))
              (define av (exact-floor (/ (+ r g b) 3)))
              (list a av av av)))))
  (argb-pixels->pict gray-buf (pict-width s)))

(define the-grr-clip
  (map (compose frame grayscale-pict) the-rr-clip))

(define the-grall-drop
  (map (compose frame grayscale-pict) the-ball-drop))

(define (slice lst n m)
  (take (drop lst n) (- m n)))
