#lang video

(multitrack
 blocks
 circ
 red
 #:transitions (list (composite-transition 0.1 0.1 0.3 0.3
                                           #:top circ
                                           #:bottom blocks)
                     (composite-transition 0.6 0.6 0.2 0.2
                                           #:top red
                                           #:bottom blocks)))
                     

(define blocks (clip "vid.mp4" #:start 50 #:end 200))
(define circ (image "circ.png" #:length 200))
(define red (color "red"))
