#lang video

(require rackunit
         racket/path
         racket/file
         syntax/location
         video/lib
         "test-utils.rkt"
         (prefix-in debug: "../private/video.rkt")
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
(define b8 (color "blue" #:length 8))
(define g (color "green"))
(define g1 (color "green" #:length 1))
(define bg (color "white"))
(define shapes (playlist circ-img vid-clip))
(define colors (playlist (color "red") (color "blue")))

;; basic examples -------------------------------------------------------------

(check-producer g #:len #f)
(check-producer g1 #:len 1)

(check-transition (composite-transition 0 0 3/4 3/4))
;(check-transition (swipe-transition #:direction 'up #:length 2)) ; TODO
(check-transition (fade-transition #:length 2))

(check-producer b8 #:len 8)
(check-producer
 (image circ-png #:length (/ (producer-length b8) 8))
   #:len 1)

(check-producer
 (multitrack
  (image circ-png #:length (/ (producer-length b8) 8))
  (composite-transition 0 0 3/4 3/4)
  b8
  #:length 5)
 #:len 5)

(check-producer (clip vid-mp4 #:length 3) #:len 3)

(check-producer (color "blue" #:length 2) #:len 2)
(check-producer (clip vid-mp4 #:start 100 #:end 103) #:len 3)
(check-producer (image circ-png #:length 1) #:len 1)
(check-producer (blank 2) #:len 2)
(check-producer (blank #f) #:len #f)
(check-producer (blank 6) #:len 6)
(check-producer circ-img #:len #f)
(check-producer circ-img #:len #f)
(check-producer vid-clip #:len 139) ; 166?
(check-producer (playlist circ-img vid-clip) #:len #f)
(check-producer (playlist (blank 2) circ-img vid-clip) #:len #f)

(check-producer shapes #:len #f)
(check-producer colors #:len #f)

;; playlists
(check-producer (playlist shapes colors) #:len #f)
(check-producer (playlist (color "green" #:length 1) (color "blue" #:length 8))
                 #:len 9)
(check-producer (playlist (playlist g1) (playlist b8)) #:len 9)


#|
TODO: bug in mlt, should be 4
(check-producer
 (playlist (image circ-png #:length 3)
           (fade-transition #:length 2)
           (clip vid-mp4 #:length 3))
 #:len 4)
|#
(check-producer
 (playlist (image circ-png #:length 4)
           (fade-transition #:length 2)
           (clip vid-mp4 #:length 4))
 #:len 6)
(check-producer
 (playlist (image circ-png #:length 3)
           (clip vid-mp4 #:length 3))
 #:len 6)

(check-producer
 (playlist
  (image circ-png #:length 4)
  (fade-transition #:length 1)
  (color "blue" #:length 5)
  (fade-transition #:length 2)
  (clip vid-mp4 #:start 0 #:end 8))
 #:len 14)

;; multitracks
(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 3/4 3/4)
  (image circ-png))
 #:len 139)


(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 1/2 1/2)
  (multitrack
   (image circ-png)
   (composite-transition 0 1/2 1/2 1/2)
   (color "green")))
 #:len 139)

;; explicit transition list
(check-producer
 (multitrack
  circ-img vid-clip b g
  #:transitions
  (list (composite-transition 0 0 1/2 1/2 #:top circ-img #:bottom vid-clip)
        (composite-transition 1/2 0 1/2 1/2 #:top b #:bottom vid-clip)
        (composite-transition 0 1/2 1/2 1/2 #:top g #:bottom vid-clip)))
 #:len 139)

(check-producer (fading-playlist (image circ-png) (color "green")))
(check-producer (fading-playlist (color "green") (clip vid-mp4)))
(define (fading-playlist a b)
  (playlist a b
            #:transitions
            (list
             (composite-transition 0 0 1/2 1/2
                                   #:top a
                                   #:bottom b))))

;; filters
(check-producer (attach-filter (image circ-png) (scale-filter 1 3)))


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
(external-video 'green)
;(external-video (quote-module-path green))
(external-video "green.vid")
#;
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
  (color "red" #:length 500)
  (color "blue" #:length 500)))
;(define bg (color "blue")) ; already defined

;; TODO, put defines at end?
(define (make-talk-video main-talk)
  ;; defines should be after playlist?
  (define begin-clip (image circ-png #:length 500))
  (define end-clip (image circ-png #:length 500))
  (playlist begin-clip
            (fade-transition #:length 200)
            main-talk
            (fade-transition #:length 200)
            end-clip))
(check-producer
 (make-talk-video (color "red" #:length 1000))
 #:len 1600)

; TODO: add filters
(define (attach-audio v a o)
  (define cleaned-audio
    (cut-producer a #:end o))
  (multitrack v cleaned-audio #:length (producer-length v)))
(check-producer
 (attach-audio (color "red" #:length 100) (color "blue" #:length 100) 50)
 #:len 100)

;; TODO: use define*
(define (make-conf-talk sp sl a o)
  (define X (make-speaker-slides-composite sp sl))
  (define Y (make-talk-video X))
  (define v (make-talk-video Y))
  (attach-audio v a o))

#|
(check-producer
 (make-conf-talk (blank 100) (blank 100) (blank 100) 0))

|#

;; Just test by running it
(debug:debug/save-prop circ-img (make-temporary-file))

(check-equal? (producer-length/unedited
               (clip vid-mp4 #:start 50 #:end 75))
              139)

(check-equal? (playlist-clip-start
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50))
               1)
              100)

(check-equal? (playlist-clip-length
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50))
               1)
              50)

(check-equal? (playlist-clip-count
               (playlist (color "red" #:length 100)
                         (color "blue" #:length 50)))
              2)
