#lang racket/base

(require racket/cmdline
         "player.rkt")

(define output-type (make-parameter #f))
(define output-file (make-parameter #f))
(define output-width (make-parameter #f))
(define output-height (make-parameter #f))

(module+ main
  (define video-file
    (command-line
     #:program "video"
     #:once-each
     [("-t" "--type") type
                      "Output type"
                      (output-type type)]
     [("-o" "--out") out
                     "Output file"
                     (output-file out)]
     [("-w" "--width") width
                       "Video width"
                       (output-width width)]
     [("-l" "--height") height
                        "Video height"
                        (output-height height)]
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))

  (preview video))
