#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(provide video-player%
         preview)
(require (except-in racket/class field)
         racket/gui/base
         images/icons/style
         images/icons/control
         ffi/unsafe/atomic
         graph
         racket/file
         video/render
         "private/video-canvas.rkt"
         (prefix-in ffmpeg: "private/ffmpeg-pipeline.rkt")
         (prefix-in video: "private/video.rkt"))

;; Constants for the window. These 'should' be setable by whoever
;; is previewing the video.
(define WIDTH 640)
(define HEIGHT 480)
(define FPS 25)

;; Probably not threadsafe when changing videos?
;; Sadly not entirely sure.
(define video-player%
  (class frame%
    (init-field video)
    (super-new [label "Playback Controls"]
               [spacing 10]
               [stretchable-width #f]
               [stretchable-height #f]
               [min-width 700]
               [min-height 600])

    ;; Internal State
    (define render-settings (make-render-settings #:start 0
                                                  #:width WIDTH
                                                  #:height HEIGHT
                                                  #:fps FPS))
    (define render #f)
    (define/public (set-video v)
      (set! render (make-object (video-canvas-render-mixin render%) video))
      (send render setup render-settings)
      (send render set-canvas screen))
    ;; Player Backend
    (define/public (get-video-length)
      (send render get-length))
    (define/public (play)
      (send render start-rendering))
    (define/public (is-stopped?)
      (send render rendering?))
    (define/public (pause)
      (send render set-speed 0))
    (define/public (stop)
      (send render stop-rendering))
    (define/public (seek position)
      (send render seek position))
    (define/public (set-speed speed)
      (send render set-speed speed))
    (define/public (rewind)
      (set-speed -5))
    (define/public (fast-forward)
      (set-speed 5))
    (define/public (get-position)
      (send render get-current-position))
    (define/public (get-fps)
      FPS)
    (define/override (show show?)
      (unless show?
        (send seek-bar-updater stop)
        (stop))
      (super show show?))
    (define/augment (on-close)
      (send seek-bar-updater stop)
      (stop))
    (define step-distance 100)

    ;; GUI For Player
    (define screen-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (define screen
      (new video-canvas%
           [parent screen-row]
           [width WIDTH] 
           [height HEIGHT]))
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
      (define len (or (get-video-length) 1000000))
      (define frame (floor (* seek-bar-max (/ (get-position) len))))
      (send seek-bar set-value frame)
      (send seek-message set-label (make-frame-string frame len)))
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
         [label (format "FPS: ~a" (get-fps))])

    ;; Start the initial video
    (set-video video)
    (define seek-bar-updater
      (new timer%
           [interval 1000]
           [notify-callback update-seek-bar-and-labels]))))

(define (preview clip)
  (define vp
    (new video-player%
         [video clip]))
  (send vp show #t)
  (send vp play)
  vp)
