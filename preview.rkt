#lang racket/base

(provide video-player%)
(require (except-in racket/class field)
         (prefix-in class: racket/class)
         racket/gui/base
         images/icons/style
         images/icons/control
         "render.rkt"
         (prefix-in render: "render.rkt")
         "lib.rkt"
         "private/mlt.rkt" ; :(, we should remove this
         "private/video.rkt")

(define video-player%
  (class frame%
    (init-field video)
    (super-new [label "Playback Controls"]
               [spacing 10]
               [stretchable-width #f]
               [stretchable-height #f]
               [min-width 500]
               [min-height 100])
    (define internal-video
      (make-link #:source video
                 #:target (make-consumer)))
    (convert-to-mlt! internal-video)
    (define play-time (producer-length video))
    (define/public (play)
      (define v (video-mlt-object internal-video))
      (when (mlt-consumer-is-stopped v)
        (mlt-consumer-start (video-mlt-object internal-video)))
      (mlt-producer-set-speed (video-mlt-object video) 1.0))
    (define/public (stop)
        (mlt-consumer-stop (video-mlt-object internal-video)))
    (define/public (seek frame)
      (define frame* (max 0 (inexact->exact (round frame))))
      (mlt-producer-seek (video-mlt-object video) frame*))
    (define/public (set-speed speed)
      (define speed* (exact->inexact speed))
      (mlt-producer-set-speed (video-mlt-object video) speed*))
    (define/public (rewind)
      (set-speed -2))
    (define/public (fast-forward)
      (set-speed 2))
    (define/public (get-position)
      (mlt-producer-position (video-mlt-object video)))
    (define/public (get-fps)
      (mlt-producer-get-fps (video-mlt-object video)))
    (define step-distance (* (get-fps) 20))
    (define top-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (new button%
         [parent top-row]
         [label (step-back-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (seek (- (get-position) step-distance)))])
    (new button%
         [parent top-row]
         [label (rewind-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (rewind))])
    (new button%
         [parent top-row]
         [label (play-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (play))])
    (new button%
       [parent top-row]
       [label (stop-icon #:color halt-icon-color #:height 50)]
       [callback (λ _ (stop))])
    (new button%
         [parent top-row]
         [label (fast-forward-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (fast-forward))])
    (new button%
         [parent top-row]
         [label (step-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (seek (+ (get-position) step-distance)))])
    (define seek-bar
      (new slider%
           [parent this]
           [label "Play Time"]
           [min-value 0]
           [max-value play-time]
           [min-width 500]
           [callback
            (λ (b e)
              (define frame (send b get-value))
              (seek frame))]))
    (new timer%
         [interval 100]
         [notify-callback
          (λ ()
            (send seek-bar set-value (get-position)))])
    (define seek-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]))
    (define seek-field
      (new text-field%
           [parent seek-row]
           [label "Seek to:"]
           [init-value "0"]
           [callback
            (λ (b e)
              (define v (send b get-value))
              (unless (string->number v)
                (send b set-value (regexp-replace* #rx"[^0-9]+" v ""))))]))
    (new button%
         [parent seek-row]
         [label "Jump"]
         [callback
          (λ (b e)
            (define frame (send seek-field get-value))
            (seek (string->number frame)))])
    (define speed-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]))
    (define speed-field
      (new text-field%
           [parent speed-row]
           [label "Play Speed:"]
           [init-value "1"]
           [callback
            (λ (b e)
              (define speed (send b get-value))
              (if (string->number speed)
                  (send b set-field-background (make-object color% "white"))
                  (send b set-field-background (make-object color% "lightsalmon"))))]))
    (new button%
          [parent speed-row]
          [label "Set Speed"]
          [callback
           (λ (b e)
             (define speed (string->number (send speed-field get-value)))
             (when speed
               (set-speed speed)))])
    (new message%
         [parent this]
         [label (format "FPS: ~a" (get-fps))])))
