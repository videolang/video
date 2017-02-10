#lang video

(color "red" #:length 75)
(fade-transition #:length 25)
(clip "vid.mp4" #:in 50 #:out 100)
(fade-transition #:length 25)
(color "blue" #:length 75)
