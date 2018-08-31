#lang racket/base

#|
   Copyright 2016-2018 Leif Andersen

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

;; This is a small library for creating video editing utilities. The
;;   main one is a video player. It also provides a video capture tool.

(require (except-in racket/class field)
         racket/contract/base
         racket/gui/base
         racket/format
         images/icons/style
         images/icons/control
         ffi/unsafe/atomic
         graph
         racket/file
         racket/match
         racket/set
         (prefix-in vidbase: "base.rkt")
         "convert.rkt"
         "render.rkt"
         "devices.rkt"
         "private/video-canvas.rkt"
         (prefix-in ffmpeg: "private/ffmpeg-pipeline.rkt")
         (prefix-in video: "private/video.rkt"))
(provide video-player-server%
         video-player%
         video-canvas%
         (contract-out
          [preview (->* (any/c)
                        (#:convert-database (or/c (is-a?/c convert-database%) #f))
                        (is-a?/c video-player%))]
          [record (->* ()
                       (#:convert-database (or/c (is-a?/c convert-database%) #f))
                       (is-a?/c video-capture%))]))

(define pause-icon-color "yellow")

;; Constants for the window. These 'should' be setable by whoever
;; is previewing the video.
(define WIDTH 640)
(define HEIGHT 480)
(define FPS 25)

(define video-player-server%
  (class object%
    (init-field [[iv video]]
                [convert-database #f]
                [(ic canvas) #f])
    (super-new)
    ;; Internal State
    (define render-settings (make-render-settings #:start 0
                                                  #:width WIDTH
                                                  #:height HEIGHT
                                                  #:fps FPS))
    (define video #f)
    (define render #f)
    (define screen #f)
    (define current-speed 1)
    (define/public (set-canvas c)
      (set! screen c)
      (when video
        (set-video video)))
    (define/public (set-video v)
      (set! video v)
      (set! render (new (video-canvas-render-mixin render%)
                        [source video]
                        [convert-database convert-database]
                        [canvas screen])))
    (define/public (get-video-length)
      (when (is-stopped?)
        (play)
        (stop))
      (send render get-length))
    (define/public (play)
      (cond
        [(is-paused?)
         (send render resume-rendering)]
        [else
         (send render setup render-settings)
         (send render start-rendering)]))
    (define/public (get-status)
      (cond [(send render rendering?)
             (cond [(or (= current-speed 0) (is-paused?))
                    'paused]
                   [(= current-speed 1) 'playing]
                   [(< current-speed 0) 'rewinding]
                   [(> current-speed 1) 'fast-forwarding]
                   [else 'playing-slow])]
            [else 'stopped]))
    (define/public (is-stopped?)
      (not (send render rendering?)))
    (define/public (is-paused?)
      (send render paused?))
    (define/public (pause)
      (send render pause-rendering))
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
      (send render render-video val))
    
    (set-canvas ic)
    (set-video iv)

    (will-register video-player-server%-executor this video-player-server%-final)))

(define (video-player-server%-final this)
  (send this stop))

(define video-player-server%-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute video-player-server%-executor)
      (loop)))))

;; The default `slider%` object is not powerful enough to determine if
;;   the user is manually dragging the slider. This is problematic for
;;   a slider that acts as a progress indicator as well.
(define video-slider%
  (class slider%
    (init [[mv- min-value]]
          [[mv+ max-value]])
    (define min-value mv-)
    (define max-value mv+)
    (super-new [min-value mv-]
               [max-value mv+])
    ;; Set of pressed buttons, is the powerset of:
    ;; (Setof 'left 'right 'middle)
    (define pressed (mutable-set))

    ;; When a button is pressed, at it to the pressed set.
    ;; Likewise remove it when released
    (define/override (on-subwindow-event r e)
      (if (send e get-left-down)
          (set-add! pressed 'left)
          (set-remove! pressed 'left))
      (if (send e get-right-down)
          (set-add! pressed 'right)
          (set-remove! pressed 'right))
      (if (send e get-middle-down)
          (set-add! pressed 'middle)
          (set-remove! pressed 'middle))
      (super on-subwindow-event r e))

    ;; Variant that should make the value exact, and also
    ;;   min/max out, in case of erronious negative time stamps.
    (define/override (set-value val)
      (define cleaned-val
        (inexact->exact
         (min max-value (max min-value val))))
      (super set-value cleaned-val))

    ;; Return #t if the user is actively sliding the slider, #f otherwise.
    (define/public (dragging?)
      (not (set-empty? pressed)))))

;; Probably not threadsafe when changing videos?
;; Sadly not entirely sure.
(define video-player%
  (class frame%
    (init-field video
                [convert-database #f])
    (super-new [label "Video Player"]
               [spacing 10]
               [stretchable-width #f]
               [stretchable-height #f]
               [min-width 700]
               [min-height 600])

    (define vps (new video-player-server%
                     [video video]
                     [convert-database convert-database]))

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
         [enabled #f]
         [label (step-back-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (send vps seek (- (send vps get-position) step-distance)))])
    (new button%
         [parent top-row]
         [enabled #f]
         [label (rewind-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (send vps rewind))])
    (define play-label (play-icon #:color run-icon-color #:height 50))
    (define pause-label (pause-icon #:color pause-icon-color #:height 50))
    (define user-stopped? #f)
    (define play/pause-button
      (new button%
           [parent top-row]
           [label (play-icon #:color run-icon-color #:height 50)]
           [callback (λ _
                       (set! user-stopped? #f)
                       (match (send vps get-status)
                         ['playing (send vps pause)]
                         [_ (send vps play)]))]))
    (new button%
       [parent top-row]
       [label (stop-icon #:color halt-icon-color #:height 50)]
       [callback (λ _
                   (set! user-stopped? #t)
                   (send vps stop))])
    (new button%
         [parent top-row]
         [enabled #f]
         [label (fast-forward-icon #:color syntax-icon-color #:height 50)]
         [callback (λ _ (send vps fast-forward))])
    (new button%
         [parent top-row]
         [enabled #f]
         [label (step-icon #:color run-icon-color #:height 50)]
         [callback (λ _ (send vps seek (+ (send vps get-position) step-distance)))])
    ;; Measured in 100 milliseconds chunks.
    (define seek-bar-max 1000000)
    (define seek-bar
      (new video-slider%
           [parent this]
           [label "Play Time"]
           [min-value 0]
           [max-value seek-bar-max]
           [min-width 500]
           [style '(plain horizontal)]
           [callback
            (λ (b e)
              (define v (send b get-value))
              (define frame (* (send vps get-video-length) (/ v seek-bar-max)))
              (send vps seek frame))]))
    (define (update-seek-bar-and-labels)
      (define status (send vps get-status))
      (define len (or (send vps get-video-length) 1000000))
      (define frame
        (cond
          [(equal? status 'playing) (send vps get-position)]
          [user-stopped?            0]
          [else                     len]))
      (define seek-pos (floor (* seek-bar-max (/ frame len))))
      (unless (eq? status 'paused)
        (send len-message set-label (make-frame-string len))
        (send seek-message set-label (make-frame-string frame)))
      (match status
        ['playing
         (unless (send seek-bar dragging?)
           (send seek-bar set-value seek-pos))
         (send play/pause-button set-label pause-label)]
        ['paused
         (send play/pause-button set-label play-label)]
        [_
         (send seek-bar set-value seek-pos)
         (send play/pause-button set-label play-label)]))
    (define/private (make-frame-string frame)
      (cond [(= frame +inf.0)
             "99:99:99.99"]
            [else
             (define frame-int (floor frame))
             (define seconds (modulo frame-int 60))
             (define minutes (modulo (floor (/ frame-int 60)) 60))
             (define hours (floor (/ frame-int 60 60)))
             (define split-secs (floor (* 100 (- frame frame-int))))
             (format "~a:~a:~a.~a"
                     (~r hours)
                     (~r minutes #:pad-string "0" #:min-width 2)
                     (~r seconds #:pad-string "0" #:min-width 2)
                     (~r split-secs #:pad-string "0" #:min-width 2))]))
    (define frame-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [horiz-margin 10]))
    (define seek-message
      (new message%
           [parent frame-row]
           [label "00:00:00.00"]
           [font (make-object font% 32 'modern 'normal 'bold)]
           [stretchable-width #f]))
    (new message%
         [parent frame-row]
         [label ""]
         [stretchable-width #t])
    (define len-message
      (new message%
           [parent frame-row]
           [label "00:00:00.00"]
           [font (make-object font% 32 'modern 'normal 'bold)]
           [stretchable-width #f]))
    (define render-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]))
    (new check-box%
         [parent render-row]
         [label "Render Video"]
         [value #t]
         [enabled #f]
         [callback
          (λ (c e)
            (define v (send c get-value))
            (render-video v))])
    (new check-box%
         [parent render-row]
         [label "Render Audio"]
         [value #t]
         [enabled #f]
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
         [enabled #f]
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
          [enabled #f]
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

;; Helper function to easily set up a video-player% class to play
;;   the given clip (with an optional convert database)
(define (preview clip #:convert-database [convert-database #f])
  (define vp
    (new video-player%
         [video clip]
         [convert-database convert-database]))
  (send vp show #t)
  (send vp play)
  vp)

(define video-capture%
  (class frame%
    (init-field [convert-database #f])
    (super-new [label "Video Capture"]
               [min-width 700]
               [min-height 600])

    ;; Video capture state
    (define video-choice (box #f))
    (define audio-choice (box #f))
    (define camera-choice (box #f))
    (define devices (list-input-devices))
    (define vps (new video-player-server%
                     [video (video:make-input-device)]
                     [convert-database convert-database]))
    
    ;; Grab the current feeds and determine how they should get
    ;;   converted to a single output.
    (define (update-vps!)
      (if (unbox camera-choice)
          (send vps set-video (video:make-input-device
                               #:video (unbox camera-choice)
                               #:render-settings (make-render-settings #:width 1280
                                                                       #:height 720
                                                                       #:fps 30
                                                                       #:pix-fmt '0rgb
                                                                       #:seek? #f)))
          (send vps set-video (vidbase:color "black")))
      (send vps play))

    ;; Need to change the choice and update the VPS whenever
    ;; a new device is chosen.
    (define ((set-choice! val) c e)
      (case (send c get-selection)
        [(0 #f) (set-box! val #f)]
        [else (set-box! val (send c get-string-selection))])
      (update-vps!))
    
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
    (define dev-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (define cam-source
      (new choice%
           [parent dev-row]
           [choices (list* "None" (cameras devices))]
           [label "Camera"]
           [callback (set-choice! camera-choice)]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    (define screen-source
      (new choice%
           [parent dev-row]
           [choices (list* "None" (video-devices devices))]
           [label "Screen Capture"]
           [callback (set-choice! video-choice)]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    (define aud-source
      (new choice%
           [parent dev-row]
           [choices (list* "None" (audio-devices devices))]
           [label "Audio Capture"]
           [callback (set-choice! audio-choice)]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    ;; Initialize the class
    (send vps set-canvas screen)
    (update-vps!)))

;; Helper function to set up a video-capture% device
(define (record #:convert-database [convert-database #f])
  (define recorder (new video-capture% [convert-database convert-database]))
  (send recorder show #t)
  recorder)
