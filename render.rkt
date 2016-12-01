#lang racket/base

(provide render
         play)
(require racket/match
         racket/dict
         "private/init-mlt.rkt"
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))


;; Render a video object (including the links
;; Video (#:profile string #:timeout Number) -> Void
(define (render data
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #:fps [fps 25]
                #:timeout [timeout #f])

  (define fps* (rationalize (inexact->exact fps) 1/1000000))
  (set-mlt-profile-width! profile width)
  (set-mlt-profile-height! profile height)
  (set-mlt-profile-frame-rate-den! profile (denominator fps*))
  (set-mlt-profile-frame-rate-num! profile (numerator fps*))
  (play (video-mlt-object data) #:timeout timeout))

;; Play a video that has already been converted to an MLT object
;; _mlt_properties (#:timeout Number) -> Void
(define (play target
              #:timeout [timeout #f])
  (mlt-consumer-start target)
  (let loop ([timeout timeout])
    (sleep 1)
    (when (and timeout (zero? timeout))
      (mlt-consumer-stop target))
    (unless (mlt-consumer-is-stopped target)
      (loop (and timeout (sub1 timeout))))))
