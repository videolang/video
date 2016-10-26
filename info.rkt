#lang info
(define collection "video")
(define deps '("base"
               "rackunit-lib"
               "gui-lib"
               "images-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/video.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(leif))
