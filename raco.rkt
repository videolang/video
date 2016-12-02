#lang racket/base

(require racket/cmdline
         "player.rkt")

(module+ main
  (define video-file
    (command-line
     #:program "video"
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))

  (preview video))
