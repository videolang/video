#lang s-exp syntax/module-reader
video

#:read read
#:read-syntax read-syntax
#:wrapper1 (λ (x) (list* 'vid 'values '() (x)))
#:info make-info

(require scribble/reader)

(define (make-info key default use-default)
  (case key
    [(drracket:toolbar-buttons)
     (define camera-button
       (dynamic-require 'video/private/camera-icon 'camera-button))
     (list camera-button)]
    [else (use-default key default)]))
