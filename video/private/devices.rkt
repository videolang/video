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

;; This module provides helper functions for accessing
;; I/O devices Video can make use of.

(provide (all-defined-out))
(require racket/match
         racket/set
         racket/logging
         "render-settings.rkt"
         "ffmpeg/main.rkt"
         "ffmpeg-pipeline.rkt"
         "init.rkt")

(struct input-devices (video
                       screen-capture
                       audio))
(define (mk-input-devices #:video [v '()]
                          #:screen-capture [sc '()]
                          #:audio [a '()])
  (input-devices v sc a))

(define (list-input-devices)
  (match (system-type 'os)
    ['unix
     (define vid-fmt (av-find-input-format "v4l2"))
     (define vid-ctx (avformat-alloc-context))
     (define video-found?
       (with-handlers ([exn:fail? (λ (e) #f)]
                       [exn:ffmpeg:fail? (λ (e) #f)])
         (avformat-open-input vid-ctx "" vid-fmt #f)
         #t))
     (define aud-fmt (av-find-input-format "alsa"))
     (define aud-ctx (avformat-alloc-context))
     (define audio-found?
       (with-handlers ([exn:fail? (λ (e) #f)])
         (avformat-open-input aud-ctx "" aud-fmt #f)
         #t))
     (mk-input-devices #:video (if video-found? (list-devices/avdevice vid-ctx) '())
                       #:audio (if audio-found? (list-devices/avdevice aud-ctx) '()))]
    [(or 'windows 'macosx)
     (define video-devices-str
       (match (system-type 'os)
         ['macosx "AVFoundation video devices:"]
         ['windows "DirectShow video devices"]))
     (define audio-devices-str
       (match (system-type 'os)
         ['macosx "AVFoundation audio devices:"]
         ['windows "DirectShow audio devices"]))
     (define dev-regexp
       (match (system-type 'os)
         ['macosx #rx"\\[[0-9]*\\] (.*)"]
         ['windows #rx" \"(.*)\""]))

     (define curr-list (box #f))
     (define video-list (box '()))
     (define audio-list (box '()))
  
     (flush-ffmpeg-log!)
     (with-intercepted-logging
         (λ (l)
           (match l
             [(vector level message data topic)
              (match message
                [(regexp video-devices-str)
                 (set-box! curr-list video-list)]
                [(regexp audio-devices-str)
                 (set-box! curr-list audio-list)]
                [_
                 (define dev-name (cadr (regexp-match dev-regexp message)))
                 (set-box! (unbox curr-list)
                           (cons dev-name (unbox (unbox curr-list))))])]))
       (λ ()
         (define fmt (av-find-input-format "avfoundation"))
         (define ctx (avformat-alloc-context))
         (with-handlers ([exn? (λ (e) (void))])
           (avformat-open-input ctx "" fmt (build-av-dict (hash "list_devices" "true"))))
         (flush-ffmpeg-log!))
       #:logger ffmpeg-logger
       'info
       (string->symbol "AVFoundation input device"))
     (mk-input-devices #:video (reverse (unbox video-list))
                       #:audio (reverse (unbox audio-list)))]
    [_
     (error "Not yet implemented for this platform")]))

;; Create a strean bundle out of an input device
;; nonnegative-integer nonnegative-integer render-settings -> stream-bundle
(define (devices->stream-bundle video-dev audio-dev
                                settings)
  (match settings
    [(struct* render-settings ([width width]
                               [height height]
                               [fps fps]
                               [pix-fmt pix-fmt]))
     (define os-dev
       (match (system-type 'os)
         ['macosx "avfoundation"]
         ['unix "v4l2"]
         ['windows "dshow"]
         [_
          (error "Not yet implemented for this platform")]))
     (define dev-spec
       (match (system-type 'os)
         ['macosx (format "~a:~a" (or video-dev "none") (or audio-dev "none"))]
         ['unix (or video-dev audio-dev)]
         ['windows
          (define vid-str (format "video=\"~a\"" video-dev))
          (define aud-str (format "audio=\"~a\"" audio-dev))
          (if (and vid-str aud-str)
              (format "~a:~a" vid-str aud-str)
              (or vid-str aud-str))]
         [_
          (error "Not yet implemented for this platform")]))
     (define fmt (av-find-input-format os-dev))
     (define ctx (avformat-alloc-context))
     (define input-ctx
       (avformat-open-input ctx dev-spec fmt
                            (build-av-dict
                             (let* ([r (hash)]
                                    [r (if (and width height)
                                           (hash-set r "video_size" (format "~ax~a" width height))
                                           r)]
                                    [r (if fps
                                           (hash-set r "framerate" (format "~a" fps))
                                           r)]
                                    [r (if pix-fmt
                                           (hash-set r "pixel_format" (format "~a" pix-fmt))
                                           r)])
                               r))))
     (avformat-context->stream-bundle input-ctx #f)]))
