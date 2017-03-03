#lang video

(require rackunit "test-utils.rkt")

;; tests from paper examples

;; TODO:
;; 2017-02-07: swipe transition not working
;; 2017-02-14: enable len tests by replacing check-producer? with check-producer

;; Constants for future tests -------------------------------------------------

(define circ-png "../examples/circ.png")
(define vid-mp4 "../examples/vid.mp4")
(define circ-img (image circ-png))
(define vid-clip (clip vid-mp4))
(define b8 (color "blue" #:length 8))
(define g (color "green"))
(define g1 (color "green" #:length 1))

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
#|
(check-producer (blank 2) #:len 2)
(check-producer circ-img #:len #f)
(check-producer circ-img #:len #f)
(check-producer vid-clip #:len 139)
(check-producer (playlist circ-img vid-clip) #:len #f)
(check-producer (playlist (blank 2) circ-img vid-clip) #:len #f)

;; shapes and colors defined after use
(check-producer shapes #:len +inf.0)
(check-producer colors #:len +inf.0)
(check-producer (playlist shapes colors) #:len +inf.0)
(check-producer (playlist (color "green" #:length 1) (color "blue" #:length 8))
                 #:len 9)
(check-producer (playlist (playlist g1) (playlist blue-clip)) #:len 9) ; cur: 1
(define shapes (playlist circ-img vid-clip))
(define colors (playlist (color "red") (color "blue")))

(check-producer
 (playlist (image circ-png #:length 3)
           (swipe-transition #:direction 'bottom #:duration 2)
           (fade-transition #:length 2)
           (clip vid-mp4 #:length 3))
 #:len 4) ; currently 1

(check-producer
 (playlist (image circ-png #:length 3)
           (clip vid-mp4 #:length 3))
 #:len 6)

;; mlt-playlist-mix failed
#;(check-producer
 (playlist
  (image circ-png #:length 2)
  (fade-transition #:length 1)
  (color "blue" #:length 2)
  (fade-transition #:length 2)
  (clip vid-mp4 #:start 0 #:end 2))
 #:len 5)

;; multitracks
(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 3/4 3/4)
  (image circ-png))
 #:len +inf.0)

(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 1/2 1/2)
  (multitrack
   (image circ-png)
   (composite-transition 0 1/2 1/2 1/2)
   (color "green")))
 #:len +inf.0)

;; defines are after use
(check-producer
 (multitrack
  circ bg green-color blue-color
  #:transitions
  (list (composite-transition 0 0 1/2 1/2 #:top circ #:bottom bg)
        (composite-transition 1/2 0 1/2 1/2 #:top blue-color #:bottom bg)
        (composite-transition 0 1/2 1/2 1/2 #:top green-color #:bottom bg)))
 #:len +inf.0)
(define bg (clip vid-mp4))
(define circ (image circ-png))
(define green-color (color "green"))
(define blue-color (color "blue"))

(check-producer (swiping-playlist (image circ-png) (color "green")))
(check-producer (swiping-playlist (color "green") (clip vid-mp4)))
(define swiping-playlist
  (λ (a b)
    (playlist a b
              #:transitions
              (list
               (composite-transition 0 0 1/2 1/2
                                     #:top a
                                     #:bottom b)))))

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

(external-video "green.vid")

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
;(define bg (color "blue")) ; already defined

;; TODO: should this work? (defines at end)
#;(define (make-talk-video main-talk)
  ;; defines are after playlist
  (playlist begin-clip
            (fade-transition 200)
            main-talk
            (fade-transition 200)
            end-clip)
  (define begin-clip (image circ-png #:length 500))
  (define end-clip (image circ-png #:length 500)))

(define (make-talk-video main-talk)
  ;; defines should be after playlist?
  (define begin-clip (image circ-png #:length 500))
  (define end-clip (image circ-png #:length 500))
  (playlist begin-clip
            (fade-transition #:length 200)
            main-talk
            (fade-transition #:length 200)
            end-clip))

; TODO: add filters
(define (attach-audio v a o)
  (define cleaned-audio
    ;; (attach-filter
    ;;  a
     (cut-producer a #:end o)
     #;(envelope-filter 50 #:direction 'in)
     #;(envelope-filter 50 #:direction 'out))
  (multitrack v cleaned-audio #:length (producer-length v)))

;; TODO: use define*
(define (make-conf-talk sp sl a o)
  (define X (make-speaker-slides-composite sp sl))
  (define Y (make-talk-video X))
  (define v (make-talk-video Y))
  (attach-audio v a o))

(check-producer
 (make-conf-talk (blank 100) (blank 100) (blank 100) 0))

|#