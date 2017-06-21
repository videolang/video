#lang racket/base

(require racket/class
         racket/file
         "../private/video.rkt"
         "../private/ffmpeg-pipeline.rkt"
         "../render.rkt")

(provide render-mixin)

(define render-mixin
  (mixin (render<%>) (render<%>)
    (super-new)
    (inherit-field dest-filename)
    (define dest-dir (make-temporary-file "list~a" 'directory))
    (define/override (render source)
      (define dest-filename* (or dest-filename "default"))
      (parameterize ([current-renderer this])
        (mlt-*-connect (make-consumer
                        #:type 'avformat
                        #:target (build-path dest-dir dest-filename*)
                        #:prop (hash "vcodec" "list"))
                       source)))
    (define/override (play source target start end speed timeout)
      (mlt-consumer-start target)
      (mlt-consumer-stop target))))

