#lang video

(multitrack
 blocks
 circ
 red
 #:merges (list (composite-merge 0.1 0.1 0.3 0.3
                                 #:top circ
                                 #:bottom blocks)
                (composite-merge 0.6 0.6 0.2 0.2
                                 #:top red
                                 #:bottom blocks)))
                     

(define blocks (clip "vid.mp4" #:properties (hash "start" 50 "end" 200)))
(define circ (clip "circ.png" #:properties (hash "length" 200)))
(define red (color "red"))
