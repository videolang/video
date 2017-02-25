#lang info

(define collection 'multi)

(define deps '("base"
               "rackunit-lib"
               "gui-lib"
               "draw-lib"
               "images-lib"
               "drracket-plugin-lib"
               "data-lib"
               "pict-lib"
               "wxme-lib"
               "sandbox-lib"
               "at-exp-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "gui-doc"))

(define version "0.1")
(define pkg-authors '(leif))
(define pkg-desc "Video Language")
