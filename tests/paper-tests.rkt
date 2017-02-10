#lang video
(require video/lib
         (only-in "../private/video.rkt" producer? transition?))
(require rackunit)

;; tests from paper examples

(define-syntax-rule (check-producer? p) (check-pred producer? p))
(define-syntax-rule (check-transition? p) (check-pred transition? p))

(define circ-png "../examples/circ.png")
(define vid-mp4 "../examples/vid.mp4")

;; fig1

(check-producer? (color "green"))
(check-producer? (color "green" #:length 1))
(check-producer?
 ; TODO: impl property-ref
 #;(image circ-png #:length (/ (property-ref blue-clip 'length) 8))
 ; TODO: producer-length needs renderer?
 #;(image circ-png #:length (/ (producer-length blue-clip) 8))
 (image circ-png #:length (/ (blue-length) 8)))

; TODO: why are these false?
;(check-transition? (composite-transition 0 0 3/4 3/4))
;(check-transition? (swipe-transition #:direction 'up #:length 2)) ; TODO
;(check-transition? (fade-transition #:length 2))

; TODO: impl where
(define blue-clip (color "blue" #:length 8))
(check-producer? blue-clip)
(define (blue-length) 8)

(check-producer?
 (multitrack
  (image circ-png #:length (/ (blue-length) 8))
  (composite-transition 0 0 3/4 3/4)
  blue-clip
  ;#:length 5 ; TODO: allow #:length
  ))

(check-producer? (clip vid-mp4 #:length 3))

; pg4
(check-producer? (color "green"))
(check-producer? (color "blue" #:length 2))
;(check-producer? (clip vid-mp4 #:start 100 #:end 103)) ; TODO: fix kws
(check-producer? (clip vid-mp4 #:in 100 #:out 103))
(check-producer? (image circ-png #:length 1))
;(check-producer? (blank 2)) ; TODO: should not be #f
(define circ-img (image circ-png))
(define vid-clip (clip vid-mp4))
(check-producer? circ-img)
(check-producer? vid-clip)
(check-producer? (playlist circ-img vid-clip))
(check-producer? (playlist (blank 2) circ-img vid-clip))

; TODO: impl where
(define shapes (playlist circ-img vid-clip))
(define colors (playlist (color "red") (color "blue")))
(check-producer? shapes)
(check-producer? colors)
;(check-producer? (playlist-append shapes colors)) ; TODO; playlist-append
