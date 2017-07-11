#lang video

(multitrack
 (clip "vid.mp4" #:properties (hash "start" 50 "end" 200))
 (playlist
  (blank 40)
  (color "red")))
