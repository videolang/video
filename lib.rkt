#lang racket/base

(provide (all-defined-out))
(require racket/dict
         file/convertible
         "private/video.rkt"
         "private/mlt.rkt"
         (for-syntax racket/base
                     syntax/parse))

;; Determine the length of an already converted producer.
;; Producer -> Number
(define (producer-length producer)
  (define internal (convert producer 'mlt))
  (define out (mlt-producer-get-out internal))
  (define in (mlt-producer-get-in internal))
  (- out in))

;; Determine the unedited length of an already converted producer.
;;   In and Out cut points are ignored
;; Producer -> Number
(define (producer-length/unedited producer)
  (mlt-producer-get-length (convert producer 'mlt)))

;; Get the durration of a clip length in a playslit
;; Converted-Playlist Integer -> Integer
(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (convert playlist 'mlt) index))

;; Get the start time of a clip in a playlist
;; Converted-playlist Integer -> Integer
(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (convert playlist 'mlt) index))
