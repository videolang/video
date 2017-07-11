#lang video

(color "red" #:properties (hash "length" 75))
(fade-transition 25)
(clip "vid.mp4" #:properties (hash "start" 50 "end" 100))
(fade-transition 25)
(color "blue" #:properties (hash "length" 75))
