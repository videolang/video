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

(require racket/cmdline
         racket/file
         racket/path
         racket/match
         "render.rkt"
         ;(prefix-in mp4: "render/mp4.rkt")
         ;(prefix-in jpg: "render/jpg.rkt")
         ;(prefix-in png: "render/png.rkt")
         ;(prefix-in xml: "render/xml.rkt")
         #;"player.rkt")

(define output-type (make-parameter #f))
(define output-width (make-parameter 720))
(define output-height (make-parameter 576))
(define output-start (make-parameter #f))
(define output-end (make-parameter #f))
(define output-timeout (make-parameter #f))
(define output-speed (make-parameter #f))

(define rendering-box (box #f))

(define (cmd-str->num who val)
  (define ret (string->number val))
  (unless ret
    (raise-user-error '|raco video| "The ~a parameter must be a number" val))
  ret)

(module+ main
  (define video-file
    (command-line
     #:program "video"
     #:once-each
     [("-t" "--type") type
                      "Output type"
                      (output-type type)]
     [("-w" "--width") width
                       "Video width"
                       (output-width  (cmd-str->num "--width" width))]
     [("-l" "--height") height
                        "Video height"
                        (output-height (cmd-str->num "--height" height))]
     [("-s" "--start") start
                       "Rendering start start"
                       (output-start (cmd-str->num "--start" start))]
     [("-e" "--end")  end
                      "Rendering end position"
                      (output-end (cmd-str->num "--end" end))]
     [("--timeout")   timeout
                      "Set a timeout for the renderer"
                      (output-timeout (cmd-str->num "--timeout" timeout))]
     [("--speed")     speed
                      "Set the speed of the output video"
                      (output-speed (cmd-str->num "--speed" speed))]
     #:args (video)
     video))

  (define video (dynamic-require video-file 'vid))
  (define output-dir (or (path-only video-file) (current-directory)))
  (define output-file (path-replace-extension (file-name-from-path video-file) ".mp4"))

  (define render-mixin
    #f
    #;
    (match (output-type)
      ["mp4" mp4:render-mixin]
      ["jpg" jpg:render-mixin]
      ["png" png:render-mixin]
      ["xml" xml:render-mixin]
      [_ #f]))
  
  (match (output-type)
    [_ ;(or "png" "jpg" "mp4" "xml")
     (define t
       (thread
        (λ ()
          (render video output-dir
                  #:start (output-start)
                  #:end (output-end)
                  #:width (output-width)
                  #:height (output-height)
                  #:timeout (output-timeout)
                  #:speed (output-speed)
                  #:dest-filename output-file
                  #:render-mixin render-mixin
                  #:rendering-box rendering-box))))
     (let loop ()
       (sleep 1)
       (when (thread-running? t)
         (loop)))]))
     #|
     (newline)
     (let loop ()
       (let ()
         (define r (unbox rendering-box))
         (when r
           (define len (get-rendering-length r))
           (define pos (get-rendering-position r))
           (if len
               (printf "\r~a/~a (~a%)            " pos len (* (/ pos len) 100.0))
               (displayln "Unbounded Video"))))
       (sleep 1)
       (if (thread-running? t)
           (loop)
           (newline)))]
    [_ (void (preview video))]))
|#
