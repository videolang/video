#lang racket/base

(require racket/class
         "../private/video.rkt"
         "../private/ffmpeg-pipeline.rkt"
         "../render.rkt")

(provide render-mixin)

(define render-mixin
  (mixin (render<%>) (render<%>)
    (super-new)
    (inherit-field dest-dir dest-filename)
    (define/override (render source)
      (define dest-filename* (or dest-filename "default"))
      (parameterize ([current-renderer this])
        (mlt-*-connect (make-consumer
                        #:type 'xml
                        #:target (build-path dest-dir
                                             (format "~a.xml" dest-filename*)))
                       source)))))
