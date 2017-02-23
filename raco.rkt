#lang racket/base

(require racket/cmdline
         racket/file
         racket/path
         racket/match
         "render.rkt"
         (prefix-in mp4: "mp4-render.rkt")
         (prefix-in jpg: "jpg-render.rkt")
         "player.rkt")

(define output-type (make-parameter #f))
(define output-width (make-parameter #f))
(define output-height (make-parameter #f))
(define output-start (make-parameter #f))
(define output-end (make-parameter #f))

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
     [("-s" "--start") start
                       "Rendering start start"
                       (output-start start)]
     [("-e" "--end")  end
                      "rendering end position"
                      (output-end end)]
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))
  (define output-dir (or (path-only video-file) (current-directory)))
  (define output-file (path-replace-extension (file-name-from-path video-file) ""))

  (match (output-type)
    ["mp4" (render video output-dir
                   #:dest-filename output-file
                   #:render-mixin mp4:render-mixin)]
    ["jpg" (render video output-dir
                   #:dest-filename output-file
                   #:render-mixin jpg:render-mixin)]
    [_ (preview video)]))
