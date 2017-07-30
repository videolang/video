#lang video

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
         racket/path
         racket/file
         syntax/location
         racket/class
         "test-utils.rkt"
         (prefix-in debug: "../private/video.rkt")
         "../render.rkt"
         "../private/utils.rkt")

;; tests from paper examples

;; TODO:
;; 2017-02-07: swipe transition not working
;; 2017-02-14: enable len tests by replacing check-producer? with check-producer

;; Constants for future tests -------------------------------------------------

(define circ-png (build-path video-dir "examples/circ.png"))
(define vid-mp4 (build-path video-dir "examples/vid.mp4"))
(define circ-img (image circ-png))
(define vid-clip (clip vid-mp4))
(define b (color "blue"))
(define b8 (color "blue" #:properties (hash "start" 0 "end" 8)))
(define g (color "green"))
(define g1 (color "green" #:properties (hash "start" 0 "end" 1)))
(define bg (color "white"))
(define shapes (playlist circ-img vid-clip))
(define colors (playlist (color "red") (color "blue")))

;; basic examples -------------------------------------------------------------

(check-producer g #:len +inf.0)
(check-producer g1 #:len 1)

(check-transition (composite-transition 0 0 3/4 3/4))
;(check-transition (swipe-transition #:direction 'up #:length 2)) ; TODO
(check-transition (fade-transition 2))

(check-producer b8 #:len 8)
(check-producer
 (image circ-png #:properties (hash "start" 0 "end" (/ (get-property b8 "length") 8)))
   #:len 1)
(check-producer
 (image circ-png #:properties (hash "length" (/ (get-property b8 "length") 8)))
   #:len 1)

(check-producer
 (multitrack
  (image circ-png #:properties (hash "start" 0 "end" (/ (get-property b8 "length") 8)))
  (composite-transition 0 0 3/4 3/4)
  b8
  #:properties (hash "start" 0 "end" 5))
 #:len 5)

(check-producer (clip vid-mp4 #:properties (hash "start" 0 "end" 3)) #:len 3)

(check-producer (color "blue" #:properties (hash "start" 0 "end" 2)) #:len 2)
(check-producer (clip vid-mp4 #:properties (hash "start" 100 "end" 103)) #:len 3)
(check-producer (image circ-png #:properties (hash "start" 0 "end" 1)) #:len 1)
(check-producer (blank 2) #:len 2)
(check-producer (blank #f) #:len +inf.0)
(check-producer (blank 6) #:len 6)
(check-producer circ-img #:len +inf.0)
(check-producer vid-clip #:len 83/15)
(check-producer (playlist circ-img vid-clip) #:len +inf.0)
(check-producer (playlist (blank 2) circ-img vid-clip) #:len +inf.0)

(check-producer shapes #:len +inf.0)
(check-producer colors #:len +inf.0)

(let ()
  (define vid1
    (color "green" #:properties (hash "width" 10 "height" 20)))

  (define vid2
    (color "green" #:properties (hash "width" 10 "height" 20)))
  (check-not-equal? vid1 vid2))


;; playlists
(check-producer (playlist shapes colors) #:len +inf.0)
(check-producer (playlist (color "green" #:properties (hash "length" 1))
                          (color "blue" #:properties (hash "length" 8)))
                 #:len 9)
(check-producer (playlist (playlist g1) (playlist b8)) #:len 9)


(check-producer
 (playlist (image circ-png #:properties (hash "start" 0 "end" 3))
           (fade-transition 2)
           (clip vid-mp4 #:properties (hash "start" 0 "end" 3)))
 #:len 4)

(check-producer
 (playlist (image circ-png #:properties (hash "start" 0 "end" 4))
           (fade-transition 2)
           (clip vid-mp4 #:properties (hash "start" 0 "end" 4)))
 #:len 6)

(check-producer
 (playlist (image circ-png #:properties (hash "length" 3))
           (clip vid-mp4 #:properties (hash "start" 0 "end" 3)))
 #:len 6)

(check-producer
 (playlist
  (image circ-png #:properties (hash "length" 4))
  (fade-transition 1)
  (color "blue" #:properties (hash "length" 5))
  (fade-transition 2)
  (clip vid-mp4 #:properties (hash "start" 0 "end" 8)))
 #:len 14)

;; multitracks
(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 3/4 3/4)
  (image circ-png))
 #:len 83/15)

(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 1/2 1/2)
  (multitrack
   (image circ-png)
   (composite-transition 0 1/2 1/2 1/2)
   (color "green")))
 #:len 83/15)

(check-producer
 (multitrack
  (color "green" #:properties (hash "width" 10 "height" 20))
  (composite-transition 0.5 0.5 0.25 0.25)
  (color "green" #:properties (hash "width" 10 "height" 20)))
 #:len +inf.0)

;; explicit transition list
(check-producer
 (multitrack
  circ-img vid-clip b g
  #:transitions
  (list (composite-transition 0 0 1/2 1/2 #:top circ-img #:bottom vid-clip)
        (composite-transition 1/2 0 1/2 1/2 #:top b #:bottom vid-clip)
        (composite-transition 0 1/2 1/2 1/2 #:top g #:bottom vid-clip)))
 #:len 83/15)


(check-producer (fading-playlist (image circ-png) (color "green")))
(check-producer (fading-playlist (color "green") (clip vid-mp4)))
(define (fading-playlist a b)
  (playlist a b
            #:transitions
            (list
             (composite-transition 0 0 1/2 1/2
                                   #:top a
                                   #:bottom b))))

#| TODO, finish scale
;; filters
(check-producer (attach-filter (image circ-png) (scale-filter 1 3)))
|#


;; props
(check-producer
 (multitrack
  rect-clip
  (composite-transition
   0
   (if (get-property rect-clip "bottom?") 1/2 0)
   1/2 1/2)
  (image circ-png)))
(define rect-clip (set-property (clip vid-mp4) "bottom?" #t))
(check-equal? (get-property rect-clip "bottom?") #t)

(module green video
  vid values
  (color "green"))
;(external-video 'green)
(external-video (quote-module-path green))
(external-video "green.vid")
(parameterize ([current-directory video-dir])
  (external-video "green.vid"))


;; racketcon
(define (make-speaker-slides-composite sp sl)
  (multitrack sp sl logo bg
              #:transitions
              (list (composite-transition 0 0 3/10 1 #:top sp #:bottom bg)
                    (composite-transition 0 1/2 3/10 1 #:top logo #:bottom bg)
                    (composite-transition 1/3 0 2/3 1 #:top sl #:bottom bg))))
(define logo (image circ-png))
(define sp (blank 100))
(define sl (blank 100))
(check-producer
 (make-speaker-slides-composite
  (color "red" #:properties (hash "length" 500))
  (color "blue" #:properties (hash "length" 500))))
;(define bg (color "blue")) ; already defined

(check-producer
 (playlist
  (image circ-png #:properties (hash "length" 500))
  (fade-transition 200)
  (color "red" #:properties (hash "length" 1000)))
 #:len 1300)

(check-producer
 (playlist
  (image circ-png #:properties (hash "length" 500))
  (fade-transition 200)
  (color "red" #:properties (hash "length" 1000))
  (image circ-png #:properties (hash "length" 500)))
 #:len 1800)

;; TODO, put defines at end?
(define (make-talk-video main-talk)
  ;; defines should be after playlist?
  (define begin-clip (image circ-png #:properties (hash "length" 500)))
  (define end-clip (image circ-png #:properties (hash "length" 500)))
  (playlist begin-clip
            (fade-transition 200)
            main-talk
            (fade-transition 200)
            end-clip))
(check-producer
 (make-talk-video (color "red" #:properties (hash "length" 1000)))
 #:len 1600)

; TODO: add filters
(define (attach-audio v a o)
  (define cleaned-audio
    (cut-producer a #:end o))
  (multitrack v cleaned-audio #:properties (hash "length" (get-property v "length"))))
(check-producer
 (attach-audio (color "red" #:properties (hash "length" 100))
               (color "blue" #:properties (hash "length" 100))
               50)
 #:len 100)

;; TODO: use define*
(define (make-conf-talk sp sl a o)
  (define X (make-speaker-slides-composite sp sl))
  (define Y (make-talk-video X))
  (define v (make-talk-video Y))
  (attach-audio v a o))

#| XXX, needs to be uncomented!
(check-producer
 (make-conf-talk (blank 100) (blank 100) (blank 100) 0))
|#


#|
;; TODO, rethink
(check-equal? (get-property
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50))
               "playlist-clip-start"
               1)
              100)

(check-equal? (get-property
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50))
               "playlist-clip-length"
               1)
              50)

(check-equal? (get-property
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50))
               "playlist-clip-count")
              2)
|#

(check-equal?
 (get-property
  (playlist
   (image circ-png #:properties (hash "length" 4))
   (fade-transition 2)
   (color "blue" #:properties (hash "length" 4))
   (fade-transition 2)
   (clip vid-mp4 #:properties (hash "start" 0 "end" 4)))
  "length")
 8)

(let ()
  (define r (new render% [source (multitrack
                                  (color "green")
                                  (image circ-png))]))
  (send r setup (make-render-settings)))

(let ()
  (define r (new render% [source (playlist
                                  (image circ-png #:properties (hash "length" 100))
                                  (fade-transition 10)
                                  (clip vid-mp4))]))
  (send r setup (make-render-settings)))

(let ()
  (define r (new render% [source (multitrack
                                  (clip vid-mp4)
                                  (overlay-transition 0 0 100 100)
                                  (image circ-png))]))
  (send r setup (make-render-settings)))
