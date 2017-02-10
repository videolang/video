#lang video
(require "test-utils.rkt")

;; tests from paper examples


(define circ-png "../examples/circ.png")
(define vid-mp4 "../examples/vid.mp4")

;; fig1

(define g (color "green"))
(define g1 (color "green" #:length 1))
(check-producer g #:len #f)
(check-producer g1 #:len 1)

(check-producer
 ; TODO: impl property-ref
 #;(image circ-png #:length (/ (property-ref blue-clip 'length) 8))
 ; TODO: producer-length needs renderer?
 #;(image circ-png #:length (/ (producer-length blue-clip) 8))
 (image circ-png #:length (/ (blue-length) 8))
 #:len 1)

; TODO: expose transition?
;(check-transition? (composite-transition 0 0 3/4 3/4))
;(check-transition? (swipe-transition #:direction 'up #:length 2)) ; TODO
;(check-transition? (fade-transition #:length 2))

; TODO: impl where
(define blue-clip (color "blue" #:length 8))
(check-producer blue-clip #:len (blue-length))
(define (blue-length) 8)

(check-producer?
 (multitrack
  (image circ-png #:length (/ (blue-length) 8))
  (composite-transition 0 0 3/4 3/4)
  blue-clip
  ;#:length 5 ; TODO: allow #:length
  ))

(check-producer (clip vid-mp4 #:length 3) #:len 3)

; examples starting from pg4
(check-producer (color "blue" #:length 2) #:len 2)
;(check-producer? (clip vid-mp4 #:start 100 #:end 103)) ; TODO: fix kws
(check-producer (clip vid-mp4 #:in 100 #:out 103) #:len 3)
(check-producer (image circ-png #:length 1) #:len 1)
;(check-producer? (blank 2)) ; TODO: should not be #f
(define circ-img (image circ-png))
(define vid-clip (clip vid-mp4))
(check-producer circ-img #:len #f)
(check-producer vid-clip #:len #f)
(check-producer (playlist circ-img vid-clip) #:len #f)
(check-producer (playlist (blank 2) circ-img vid-clip) #:len #f)

; TODO: impl where
(define shapes (playlist circ-img vid-clip))
(define colors (playlist (color "red") (color "blue")))
(check-producer shapes #:len #f)
(check-producer colors #:len #f)
;(check-producer? (playlist-append shapes colors)) ; TODO; playlist-append

(check-producer
 (playlist (image circ-png #:length 3)
           ;(swipe-transition #:direction 'bottom #:duration 2)
           (fade-transition #:length 2)
           (clip vid-mp4 #:length 3))
 ; #:len 8 ; TODO: fix length? is #f
 )

(check-producer
 (playlist (image circ-png #:length 3)
           (clip vid-mp4 #:length 3))
; #:len 6 ; TODO: fix length? right now is #f
 )

;; multitracks
(check-producer
 (multitrack
  (clip vid-mp4)
  (composite-transition 0 0 3/4 3/4)
  (image circ-png))
 #:len #f)

