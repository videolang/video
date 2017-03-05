#lang racket

(require rackunit
         "../private/editor.rkt")

(let ()
  (define 3ed
    (new video-editor%
         [track-height 10]
         [minimum-width 100]
         [initial-tracks 3]))
  (check-equal?
   (send 3ed get-min-height)
   30)
  (check-equal?
   (send 3ed get-min-width)
   100))

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
   500))
