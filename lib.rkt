#lang racket/base

(provide (all-defined-out))
(require racket/dict
         "private/video.rkt"
         "private/mlt.rkt"
         (for-syntax racket/base
                     syntax/parse))

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

;; Get the durration of a clip length in a playslit
;; Converted-Playlist Integer -> Integer
(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (video-mlt-object playlist) index))

;; Get the start time of a clip in a playlist
;; Converted-playlist Integer -> Integer
(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (video-mlt-object playlist) index))

;; Given a video struct, determine what its type is, and
;;   return an identifier associated with the type of struct
;; Video -> Identifier
(define (video-type video)
  (define type
    (for/fold ([acc #f])
              ([(pred type) (in-dict global-struct-type-predicate-table)]
               #:break acc)
      (and (pred video) type)))
  (or type
      (error 'video-type "~a is not a video struct" video)))
