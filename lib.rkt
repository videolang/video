#lang racket/base

(provide (all-defined-out))
(require "private/video.rkt"
         "private/mlt.rkt")

;; Tests to see if a video has been converted, generally by
;;    being passed into convert-to-mlt!
;; Video -> Boolean
(define (converted-video? video)
  (video-mlt-object video))

;; Determine the length of an already converted producer.
;; Converted-Producer -> Number
(define (producer-length producer)
  (define out (mlt-producer-get-out (video-mlt-object producer)))
  (define in (mlt-producer-get-in (video-mlt-object producer)))
  (- out in))

;; Determine the unedited length of an already converted producer.
;;   In and Out cut points are ignored
;; Converted-Producer -> Number
(define (producer-length/unedited producer)
  (mlt-producer-get-length (video-mlt-object producer)))
