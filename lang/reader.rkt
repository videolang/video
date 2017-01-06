#lang s-exp syntax/module-reader
video
#:wrapper1 (λ (x) (list* 'vid 'values '() (x)))
#:info make-info

(require "../private/res/camera-icon.rkt")

(define (make-info key default use-default)
  (case key
    [(drracket:toolbar-buttons)
     (list camera-button)]
    [else (use-default key default)]))
