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

(provide video-player-server%
         video-player%
         video-canvas%
         preview)
(require (except-in racket/class field)
         racket/gui/base
         images/icons/style
         images/icons/control
         ffi/unsafe/atomic
         graph
         racket/file
         racket/match
         video/render
         "private/video-canvas.rkt"
         (prefix-in ffmpeg: "private/ffmpeg-pipeline.rkt")
         (prefix-in video: "private/video.rkt"))

(define pause-icon-color "yellow")

;; Constants for the window. These 'should' be setable by whoever
;; is previewing the video.
(define WIDTH 640)
(define HEIGHT 480)
(define FPS 25)

(define video-player-server%
  (class object%
    (init-field video)
    (super-new)
    ;; Internal State
    (define render-settings (make-render-settings #:start 0
                                                  #:width WIDTH
                                                  #:height HEIGHT
                                                  #:fps FPS))
    (define render #f)
    (define screen #f)
    (define current-speed 1)
    (define/public (set-canvas c)
      (set! screen c)
      (set-video video))
    (define/public (set-video v)
      (set! video v)
      (set! render (make-object (video-canvas-render-mixin render%) video))
      (send render set-canvas screen))
    (define/public (get-video-length)
      (when (is-stopped?)
        (play)
        (stop))
      (send render get-length))
    (define/public (play)
      (send render setup render-settings)
      (send render start-rendering))
    (define/public (is-paused?)
      (and (send render rendering?)
           (= current-speed 0)))
    (define/public (get-status)
      (cond [(send render rendering?)
             (cond [(= current-speed 1) 'playing]
                   [(= current-speed 0) 'paused]
                   [(< current-speed 0) 'rewinding]
                   [(> current-speed 1) 'fast-forwarding]
                   [else 'playing-slow])]
            [else 'stopped]))
    (define/public (is-stopped?)
      (not (send render rendering?)))
    (define/public (pause)
      (send render set-speed 0))
    (define/public (stop)
      (send render stop-rendering))
    (define/public (seek position)
      (send render seek position))
    (define/public (set-speed speed)
      (set! current-speed speed)
      (send render set-speed speed))
    (define/public (rewind)
      (set-speed -5))
    (define/public (fast-forward)
      (set-speed 5))
    (define/public (get-position)
      (send render get-current-position))
    (define/public (get-fps)
      FPS)
    (define/public (render-audio val)
      (send render render-audio val))
    (define/public (render-video val)
      (send render render-video val))))

;; Probably not threadsafe when changing videos?
;; Sadly not entirely sure.
(define video-player%
  (class frame%
    (init-field video)
    (super-new [label "Video Player"]
               [spacing 10]
               [stretchable-width #f]
               [stretchable-height #f]
               [min-width 700]
               [min-height 600])

    (define vps (new video-player-server% [video video]))

    ;; Player Backend
    (define/override (show show?)
      (unless show?
        (send seek-bar-updater stop)
        (send vps stop))
      (super show show?))
    (define/augment (on-close)
      (send seek-bar-updater stop)
      (send vps stop))
    (define step-distance 100)
    (define/public (play)
      (send vps play))
    (define/public (pause)
      (send vps pause))
    (define/public (stop)
      (send vps stop))
    (define/public (seek pos)
      (send vps seek pos))
    (define/public (set-speed spd)
      (send vps set-speed spd))
    (define/public (render-video val)
      (send vps render-video val))
    (define/public (render-audio val)
      (send vps render-audio val))

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
    (send vps set-canvas screen)
    (define top-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (new button%
         [parent top-row]
         [label (step-back-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (send vps seek (- (send vps get-position) step-distance)))])
    (new button%
         [parent top-row]
         [label (rewind-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (send vps rewind))])
    (define play-label (play-icon #:color run-icon-color #:height 50))
    (define pause-label (pause-icon #:color pause-icon-color #:height 50))
    (define play/pause-button
      (new button%
           [parent top-row]
           [label (play-icon #:color run-icon-color #:height 50)]
           [callback (λ _ (match (send vps get-status)
                            ['playing (send vps pause)]
                            [_ (send vps play)]))]))
    (new button%
       [parent top-row]
       [label (stop-icon #:color halt-icon-color #:height 50)]
       [callback (λ _ (send vps stop))])
    (new button%
         [parent top-row]
         [label (fast-forward-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (send vps fast-forward))])
    (new button%
         [parent top-row]
         [label (step-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (send vps seek (+ (send vps get-position) step-distance)))])
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
              (define frame (floor (* (send vps get-video-length) (/ v seek-bar-max))))
              (send vps seek frame))]))
    (define (update-seek-bar-and-labels)
      (define len (or (send vps get-video-length) 1000000))
      (define frame (floor (* seek-bar-max (/ (send vps get-position) len))))
      (send seek-bar set-value frame)
      (send seek-message set-label (make-frame-string frame len))
      (send play/pause-button set-label (if (eq? (send vps get-status) 'playing)
                                            pause-label
                                            play-label)))
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
    (define render-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]))
    (new check-box%
         [parent render-row]
         [label "Render Video"]
         [value #t]
         [callback
          (λ (c e)
            (define v (send c get-value))
            (render-video v))])
    (new check-box%
         [parent render-row]
         [label "Render Audio"]
         [value #t]
         [callback
          (λ (c e)
            (define v (send c get-value))
            (render-audio v))])
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
            (send vps seek (string->number frame)))])
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
               (send vps set-speed speed)))])
    (new message%
         [parent this]
         [label (format "FPS: ~a" (send vps get-fps))])

    ;; Start the initial video
    (send vps set-video video)
    (define seek-bar-updater
      (new timer%
           [interval 50]
           [notify-callback update-seek-bar-and-labels]))))

(define (preview clip)
  (define vp
    (new video-player%
         [video clip]))
  (send vp show #t)
  (send vp play)
  vp)
