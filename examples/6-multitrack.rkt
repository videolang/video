#lang video

(multitrack
 (clip "vid.mp4" #:in 50 #:out 200)
 (playlist
  (blank 40)
  (color "red")))
