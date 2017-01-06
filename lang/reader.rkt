#lang s-exp syntax/module-reader
video
#:wrapper1 (λ (x) (list* 'vid 'values '() (x)))
#:info make-info

(define (make-info key default use-default)
  (case key
    [(drracket:toolbar-buttons)
     (define camera-button
       (dynamic-require "private/res/camera-icon.rkt" 'camera-button))
     (list camera-button)]
    [else (use-default key default)]))
