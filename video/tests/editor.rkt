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
         racket/gui/base
         wxme
         "../private/editor.rkt")

(let ()
  (define 3ed
    (new video-editor%
         [track-height 10]
         [minimum-width 100]
         [initial-tracks 3]))
  (define new-3ed
    (send 3ed copy-self))
  (check-equal?
   (send 3ed get-min-height)
   30)
  (check-equal?
   (send 3ed get-min-width)
   100)
  (check-equal?
   (send new-3ed get-min-height)
   30)
  (check-equal?
   (send new-3ed get-min-width)
   100)
  (check-equal?
   (syntax->datum (send new-3ed read-special #f #f #f #f))
   (syntax->datum (send 3ed read-special #f #f #f #f))))

(let ()
  (define ned
    (new video-editor%
         [initial-tracks 2]
         [track-height 200]))
  (send ned add-track)
  (check-equal?
   (send ned get-min-height)
   600)
  (send ned delete-track 1)
  (check-equal?
   (send ned get-min-height)
   400))

(let ()
  (define ed
    (new video-editor%))
  (send ed on-default-event (new mouse-event% [event-type 'right-down]))
  (define text-ed
    (new video-text%))
  (send text-ed on-event (new mouse-event% [event-type 'right-down]))
  (define file-ed
    (new video-file%))
  (send file-ed on-event (new mouse-event% [event-type 'right-down])))

(let ()
  (define ed
    (new video-editor% [initial-tracks 2]))
  (send ed insert-video (new video-snip%) 0 5 10)
  (send ed delete-track 0))

(let ()
  (define 4ed
    (new video-editor%
         [track-height 200]
         [minimum-width 500]
         [initial-tracks 4]))
  (check-equal?
   (send 4ed get-min-height)
   800)
  (check-equal?
   (send 4ed get-min-width)
   500)
  (define 4ed-str-out
    (new editor-stream-out-bytes-base%))
  (send 4ed write-to-file (make-object editor-stream-out% 4ed-str-out))
  (define 4ed-str
    (send 4ed-str-out get-bytes))
  (define 4ed-str-in
    (make-object editor-stream-in-bytes-base% 4ed-str))
  (define new-4ed
    (new video-editor%))
  (send new-4ed read-from-file (make-object editor-stream-in% 4ed-str-in))
  (check-equal?
   (send new-4ed get-min-height)
   800)
  (check-equal?
   (send new-4ed get-min-width)
   500))

(let ()
  (define vf (new video-file% [file (build-path "hello.mp4")]))
  (send vf set-file! (build-path "world.mp4"))
  (define vf2 (send vf copy-self))
  (check-equal? (syntax->datum (send vf2 read-special #f #f #f #f))
                (syntax->datum (send vf read-special #f #f #f #f)))
  (check-equal? (send vf2 get-file)
                (send vf get-file)))

(let ()
  (define vs (new video-snip%
                  [editor (new video-editor%)]))
  (define vs2 (send vs copy))
  (define b1 (new editor-stream-out-bytes-base%))
  (define b2 (new editor-stream-out-bytes-base%))
  (send vs2 write (make-object editor-stream-out% b2))
  (send vs write (make-object editor-stream-out% b1))
  (check-equal? (send b1 get-bytes)
                (send b2 get-bytes))
  (define b3 (make-object editor-stream-in-bytes-base% (send b1 get-bytes)))
  (define b4 (make-object editor-stream-in-bytes-base% (send b1 get-bytes)))
  (define b5 (make-object editor-stream-in-bytes-base% (send b1 get-bytes)))
  (define vr (new video-snip-reader%))
  ;(define wr (wxme-read (open-input-bytes (send b1 get-bytes))))
  ;(displayln wr)
  ;(define vs3 (send vr read-snip #f "0" (make-object editor-stream-in% b3)))
  ;(define vt (send vr read-snip #t "0" (make-object editor-stream-in% b4)))
  ;(displayln vt)
  (check-equal? (send vr read-header "0" (make-object editor-stream-in% b5))
                (void)))

