#lang racket

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

(require rackunit
         racket/gui/base
         "../player.rkt"
         "../base.rkt"
         "../private/video-canvas.rkt"
         "../private/utils.rkt"
         (prefix-in green: "green.vid"))

(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

(let ()
  (define f (new frame% [label "foo"]))
  (define c (new video-canvas%
                 [width 640]
                 [height 480]
                 [parent f]))
  (send f show #t)
  (define p (new video-player-server% [video green:vid]))
  (send p set-canvas c)
  (send p play)
  (check-true (>= (send p get-video-length) 9999))
  (send p stop)
  (check-true (send p is-stopped?))
  (check-false (send p is-paused?))
;  (send p seek 10)
;  (check-equal? (send p get-position) 10)
  (check-equal? (send p get-fps) 25)
  (send f show #f))

(let ()
  (define p (new video-player% [video green:vid]))
  (send p show #t)
  (send p play)
  (send p stop)
  (send p show #f))

(let ()
  (define p (preview green:vid))
  (send p stop)
  (send p show #f))

(let p ()
  (define p (preview green:vid))
  (sleep 3)
  (send p stop)
  (send p show #f))

(let ()
  (define p (new video-player% [video green:vid]))
  (send p show #t)
  (send p render-audio #f)
  (send p play)
  (sleep 1)
  (send p stop)
  (send p show #f))

(let ()
  (define p (preview (clip vid-mp4)))
  (sleep 3)
  (send p stop)
  (send p show #f))

;; Some stress tests
(let ()
  (define vid
    (multitrack (clip vid-mp4)
                (overlay-merge 50 50 100 100)
                (color "green")))
  (define p (preview vid))
  (send p stop)
  (for ([i (in-range 10)])
    (send p play)
    (sleep 2)
    (send p stop)
    (sleep 0.5))
  (send p show #f))
