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

;; XXX This library only works on macOS, must fix!

(provide (all-defined-out))
(require racket/match
         racket/set
         racket/logging
         "ffmpeg/main.rkt"
         "ffmpeg-pipeline.rkt"
         "init.rkt")

(struct device-list (video
                     audio)
  #:transparent)

(define video-devices-str "AVFoundation video devices:")
(define audio-devices-str "AVFoundation audio devices:")
(define dev-regexp #rx"\\[[0-9]*\\] (.*)")

(define (list-input-devices)
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
  (device-list (reverse (unbox video-list))
               (reverse (unbox audio-list))))
