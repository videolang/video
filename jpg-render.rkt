#lang racket/base

(require racket/class
         "private/video.rkt"
         "private/mlt.rkt"
         "render.rkt")

(provide render-mixin)

(define render-mixin
  (mixin (render<%>) (render<%>)
    (super-new)
    (inherit-field dest-dir dest-filename)
    (define/override (render source)
      (define dest-filename* (or dest-filename "default"))
      (parameterize ([current-renderer this])
        (mlt-*-connect (make-consumer
                        #:type 'avformat
                        #:target (build-path dest-dir (format "~a%05d.jpg" dest-filename*))
                        #:prop (hash "vcodec" "jpeg2000"))
                       source)))))
