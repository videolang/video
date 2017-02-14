#lang video

(define v (clip "vid.mp4"))
(cut-producer v #:start 50)
