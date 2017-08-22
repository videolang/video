#lang racket

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

(require rackunit
         racket/logging
         racket/pretty
         (prefix-in green: "green.vid")
         (prefix-in video: "../render.rkt")
         (submod "../render.rkt" render-fields)
         (prefix-in video: "../base.rkt")
         "../private/ffmpeg/main.rkt"
         "../private/ffmpeg-pipeline.rkt"
         "../private/init.rkt"
         "../private/utils.rkt"
         "../private/log.rkt"
         "test-utils.rkt")

(define-namespace-anchor here-anchor)
(define here (namespace-anchor->namespace here-anchor))
(define vid-mp4 (build-path video-dir "examples/vid.mp4"))

;; Give the logging code a dry run to ensure
;;   that it doesn't crash anything.
;; Because this is just the info log, we don't
;;   really care about what is or is not in there.
(with-intercepted-logging (λ (l) (void))
  (λ ()
    (video:render green:vid
                  (make-temporary-file "~a.mp4")
                  #:start 0
                  #:end 30))
  #:logger video-logger
  'debug
  'video)

(let ()
  (define r (make-object video:render% (video:clip vid-mp4)))
  (send r setup (video:make-render-settings #:destination (make-temporary-file "~a.mp4")))
  (define g (get-field render-graph r))
  (render g))

(let ()
  (define b (file->stream-bundle vid-mp4))
  (define ctx (stream-bundle-avformat-context b))
  (avformat-context->list ctx)
  (for ([name (in-list _avformat-context-field-names)])
    (define accessor (eval (string->symbol (format "avformat-context-~a" name)) here))
    (accessor ctx)))

(let ()
  (define c (mk-command "str0" "fade"))
  (command->string c)
  (define c* (mk-command "str1" "fade" #:flags '(enter) #:arg "arg"))
  (command->string c*)
  (void))
  
(let ()
  (define i (mk-interval 5 (list (mk-command "str0" "cat"))))
  (interval->string i)
  (define i* (mk-interval 5 (list (mk-command "str1" "app")) #:end 6))
  (interval->string i*)
  (void))

(let ()
  (define tn (mk-trim-node #:start 5 #:end 10))
  (define rsv (mk-reset-timestamp-video-filter))
  (define rsa (mk-reset-timestamp-audio-filter))
  (define rsn (mk-reset-timestamp-node))
  (void))

(let ()
  (check-equal? (color->string '(255 0 0))
                "0xff0000ff")
  (check-equal? (color->string "green")
                 "0x00ff00ff"))


(let ()
  (define b (mk-stream-bundle))
  (check-true (stream-bundle? b))
  (define ci (mk-codec-obj))
  (check-true (codec-obj? ci)))

(let ()
  (filter->string
   (mk-filter "myfilter"
              (hash "a" '("b" "c" "d")
                    "e" (mk-duration 5)
                    "f" (mk-interval 8 '()))))
  (void))

(let ()
  (convert-dict (hash "a" "b" "c" "d"))
  (void))

(let ()
  (define g (avfilter-graph-alloc))
  (filter->avfilter
   (mk-filter "trim" (hash "start" 3 "end" 42))
   g)
  (avfilter-graph-free g)
  (void))

(let ()
  (define f (file->stream-bundle vid-mp4))
  (check-equal? (maybe-av-find-best-stream (stream-bundle-avformat-context f) 'video) 0)
  (define dm (new demux% [bundle f]))
  (send dm dump-info)
  (send dm init)
  (send dm close)
  (define f2 (stream-bundle->file (make-temporary-file "~a.mp4") 'vid+aud))
  (define mx (new mux% [bundle f2]))
  (send mx dump-info))

(let ()
  (define f (stream-bundle->file (make-temporary-file "~a.mp4")
                                 (mk-stream-bundle
                                  #:streams (vector 'video
                                                    'audio
                                                    (mk-codec-obj #:type 'audio
                                                                  #:id 'aac
                                                                  #:index 3)))))
  (check-true
   (stream-bundle? f)))
