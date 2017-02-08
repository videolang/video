#lang racket/base

(require racket/cmdline
         racket/file
         racket/path
         racket/match
         "render.rkt"
         (prefix-in mp4: "mp4-render.rkt")
         "player.rkt")

(define output-type (make-parameter #f))
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
     [("-w" "--width") width
                       "Video width"
                       (output-width width)]
     [("-l" "--height") height
                        "Video height"
                        (output-height height)]
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))
  (define output-dir (or (path-only video-file) (current-directory)))
  (define output-file (path-replace-extension (file-name-from-path video-file) ""))

  (match (output-type)
    ["mp4" (render video output-dir
                   #:dest-filename output-file
                   #:render-mixin mp4:render-mixin)]
    [_ (preview video)]))
