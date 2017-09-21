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

;; This module stores the render-settings struct used by the renderer and
;;   derived classes.
;; This needs to be in its own module (rather than a submodule) to avoid
;;   a cycle when loading the module.

;; Defined in a submodule so that classes
;; extending this one can make use of it.

(provide (all-defined-out))
(struct render-settings (destination
                         width
                         height
                         start
                         end
                         fps
                         format
                         video-codec
                         audio-codec
                         subtitle-codec
                         pix-fmt
                         sample-fmt
                         sample-rate
                         channel-layout
                         speed
                         video-frames
                         audio-frames
                         data-frames
                         seek?))
(define (make-render-settings #:destination [d #f]
                              #:width [w 1920]
                              #:height [h 1080]
                              #:start [s #f]
                              #:end [e #f]
                              #:fps [f 25]
                              #:format [fo #f]
                              #:video-codec [vc #f]
                              #:audio-codec [ac #f]
                              #:subtitle-codec [sc #f]
                              #:pix-fmt [pf 'yuv420p]
                              #:sample-fmt [sf 'fltp]
                              #:sample-rate [sr 44100]
                              #:channel-layout [cl 'stereo]
                              #:speed [sp 1]
                              #:video-frames [vfr #f]
                              #:audio-frames [afr #f]
                              #:data-frames [dfr #f]
                              #:seek? [s? #t])
  (render-settings d w h s e f fo vc ac sc pf sf sr cl sp vfr afr dfr s?))
