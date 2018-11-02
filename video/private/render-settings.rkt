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

;; This module stores the render-settings struct used by the renderer and
;;   derived classes.
;; This needs to be in its own module (rather than a submodule) to avoid
;;   a cycle when loading the module.

;; Defined in a submodule so that classes
;; extending this one can make use of it.

(require racket/struct)

(provide (all-defined-out))

;; For nice printing of render settings AND video objects
(define current-detailed-printing? (make-parameter #f))

(struct render-settings (destination
                         width
                         height
                         display-aspect-ratio
                         start
                         end
                         fps
                         video-time-base
                         audio-time-base
                         format
                         video-codec
                         audio-codec
                         subtitle-codec
                         pix-fmt
                         sample-fmt
                         sample-rate
                         channel-layout
                         speed
                         render-video?
                         render-audio?
                         video-frames
                         audio-frames
                         data-frames
                         seek-point)
  #:methods gen:custom-write
  [(define (write-proc vid port mode)
     (if (current-detailed-printing?)
         ((make-constructor-style-printer
           (λ (obj) "render-settings")
           (λ (obj) (list (render-settings-destination obj)
                          (render-settings-width obj)
                          (render-settings-height obj)
                          (render-settings-display-aspect-ratio obj)
                          (render-settings-video-codec obj)
                          (render-settings-audio-codec obj)
                          (render-settings-pix-fmt obj)
                          (render-settings-sample-fmt obj)
                          (render-settings-channel-layout obj)
                          (render-settings-start obj)
                          (render-settings-end obj)
                          (render-settings-seek-point obj))))
          vid port mode)
         ((make-constructor-style-printer
           (λ (obj) "render-settings")
           (λ (obj) (list)))
          vid port mode)))])
         
(define (make-render-settings #:destination [d #f]
                              #:width [w 1920]
                              #:height [h 1080]
                              #:display-aspect-ratio [dar 16/9]
                              #:start [s #f]
                              #:end [e #f]
                              #:fps [fr #f]
                              #:video-time-base [vtb #f]
                              #:audio-time-base [atb #f]
                              #:format [fo #f]
                              #:video-codec [vc #f]
                              #:audio-codec [ac #f]
                              #:subtitle-codec [sc #f]
                              #:pix-fmt [pf 'yuv420p]
                              #:sample-fmt [sf 'fltp]
                              #:sample-rate [sr 44100]
                              #:channel-layout [cl 'stereo]
                              #:speed [sp 1]
                              #:render-video? [rv? #t]
                              #:render-audio? [rc? #t]
                              #:video-frames [vfr #f]
                              #:audio-frames [afr #f]
                              #:data-frames [dfr #f]
                              #:seek-point [seekp #f])
  (render-settings d w h dar s e fr vtb atb fo vc ac sc pf sf sr cl sp rv? rc? vfr afr dfr seekp))
