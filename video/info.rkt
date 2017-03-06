#lang info
(define collection "video")
(define scribblings '(("scribblings/video.scrbl" (multi-page) (tool))))
(define raco-commands '(("video"
                         (submod video/raco main)
                         "Preview or Render a Racket Video"
                         #f)))

(define drracket-tools '(("private/tool.rkt")))
(define drracket-tool-names '("Video"))
(define drracket-tool-icons '(#f))

(define test-omit-paths
  '("private/examples.rkt"))
