#lang info
(define collection "video")
(define deps '("base"
               "rackunit-lib"
               "gui-lib"
               "draw-lib"
               "images-lib"
               "drracket-plugin-lib"
               "data-lib"
               "pict-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "gui-doc"
                     "ppict"))
(define scribblings '(("scribblings/video.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(leif))
(define raco-commands '(("video"
                         (submod video/raco main)
                         "Preview or Render a Racket Video"
                         #f)))

(define drracket-tools '(("private/tool.rkt")))
(define drracket-tool-names '("Video"))
(define drracket-tool-icons '(#f))
