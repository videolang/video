#lang racket/base

(require racket/cmdline
         racket/file
         racket/path
         racket/match
         "render.rkt"
         (prefix-in mp4: "render/mp4.rkt")
         (prefix-in jpg: "render/jpg.rkt")
         (prefix-in png: "render/png.rkt")
         "player.rkt")

(define output-type (make-parameter #f))
(define output-width (make-parameter #f))
(define output-height (make-parameter #f))
(define output-start (make-parameter #f))
(define output-end (make-parameter #f))
(define output-timeout (make-parameter #f))
(define output-speed (make-parameter #f))

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
                      "Rendering end position"
                      (output-end end)]
     [("--timeout")   timeout
                      "Set a timeout for the renderer"
                      (output-timeout timeout)]
     [("--speed")     speed
                      "Set the speed of the output video"
                      (output-speed speed)]
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))
  (define output-dir (or (path-only video-file) (current-directory)))
  (define output-file (path-replace-extension (file-name-from-path video-file) ""))

  (define render-mixin
    (match (output-type)
      ["mp4" mp4:render-mixin]
      ["jpg" jpg:render-mixin]
      ["png" png:render-mixin]
      [_ #f]))
  
  (match (output-type)
    [(or "png" "jpg" "mp4") (render video output-dir
                              #:start (output-start)
                              #:end (output-end)
                              #:width (output-width)
                              #:height (output-height)
                              #:timeout (output-timeout)
                              #:speed (output-speed)
                              #:dest-filename output-file
                              #:render-mixin render-mixin)]
    [_ (preview video)]))
