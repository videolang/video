#lang racket/base

(provide video-player%
         preview)
(require (except-in racket/class field)
         racket/gui/base
         images/icons/style
         images/icons/control
         ffi/unsafe/atomic
         file/convertible
         "render.rkt"
         "lib.rkt"
         "private/mlt.rkt" ; :(, we should remove this
         "private/video.rkt")

;; Probably not threadsafe when changing videos?
;; Sadly not entirely sure.
(define video-player%
  (class frame%
    (init-field video)
    (super-new [label "Playback Controls"]
               [spacing 10]
               [stretchable-width #f]
               [stretchable-height #f]
               [min-width 500]
               [min-height 100])
    (define (video->internal-video v)
      (make-link #:source v
                 #:target (make-consumer)))
    (define internal-video (video->internal-video video))
    (define video-mlt (convert video 'mlt))
    (define internal-video-mlt (convert video 'mlt))
    (define/public (get-video-length)
      (producer-length video))
    (define/public (play)
      (define v internal-video-mlt)
      (when (mlt-consumer-is-stopped v)
        (mlt-consumer-start internal-video-mlt))
      (mlt-producer-set-speed video-mlt 1.0))
    (define/public (is-stopped?)
      (mlt-consumer-is-stopped internal-video-mlt))
    (define/public (pause)
      (set-speed 0))
    (define/public (stop)
      (mlt-consumer-stop internal-video-mlt))
    (define/public (seek frame)
      (define frame* (max 0 (inexact->exact (round frame))))
      (mlt-producer-seek video-mlt frame*)
      (update-seek-bar-and-labels))
    (define/public (set-speed speed)
      (define speed* (exact->inexact speed))
      (mlt-producer-set-speed video-mlt speed*))
    (define/public (rewind)
      (set-speed -5))
    (define/public (fast-forward)
      (set-speed 5))
    (define/public (get-position)
      (mlt-producer-position video-mlt))
    (define/public (get-fps)
      (mlt-producer-get-fps video-mlt))
    (define/public (set-video v)
      ;; Really should be atomic.... :/
      (call-as-atomic
       (λ ()
         (stop)
         (set! video v)
         (set! internal-video (video->internal-video v))
         (set! video-mlt (convert video 'mlt))
         (set! internal-video (convert internal-video 'mlt))
         (seek 0)
         (set-speed 1)
         (update-seek-bar-and-labels))))
    (define/augment (on-close)
      (send seek-bar-updater stop)
      (stop))
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
       [callback (λ _ (pause))])
    (new button%
         [parent top-row]
         [label (fast-forward-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (fast-forward))])
    (new button%
         [parent top-row]
         [label (step-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (seek (+ (get-position) step-distance)))])
    (define seek-bar-max 10000)
    (define seek-bar
      (new slider%
           [parent this]
           [label "Play Time"]
           [min-value 0]
           [max-value seek-bar-max]
           [min-width 500]
           [style '(plain horizontal)]
           [callback
            (λ (b e)
              (define v (send b get-value))
              (define frame (floor (* (get-video-length) (/ v seek-bar-max))))
              (seek frame))]))
    (define (update-seek-bar-and-labels)
      (define len (get-video-length))
      (define frame (floor (* seek-bar-max (/ (get-position) len))))
      (send seek-bar set-value frame)
      (send seek-message set-label (make-frame-string frame len)))
    (define seek-bar-updater
      (new timer%
           [interval 1000]
           [notify-callback update-seek-bar-and-labels]))
    (define/private (make-frame-string frame len)
      (format "Frame: ~a/~a" frame len))
    (define frame-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]))
    (define seek-message
      (new message%
           [parent frame-row]
           [label (make-frame-string 0 0)]
           [stretchable-width #t]))
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

(define (preview clip)
  (define vp
    (new video-player%
         [video clip]))
  (send vp show #t)
  (send vp play)
  vp)
