#lang racket/base

(require racket/file
         "../render.rkt"
         "../base.rkt"
         (prefix-in mp4: "../render/mp4.rkt")
         (prefix-in png: "../render/png.rkt")
         (prefix-in jpg: "../render/jpg.rkt")
         (prefix-in xml: "../render/xml.rkt")
         "../private/utils.rkt")

(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

(define the-clip
  (playlist
   (clip vid-mp4
         #:start 50
         #:end 100)))

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin mp4:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin png:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin jpg:render-mixin)

(render the-clip
        (make-temporary-file "vidtest~a" 'directory)
        #:render-mixin xml:render-mixin)
