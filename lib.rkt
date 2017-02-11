#lang racket/base

(require racket/dict
         racket/class
         racket/contract/base
         "private/video.rkt"
         "private/mlt.rkt"
         "private/utils.rkt"
         "render.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide
 (contract-out
  ;; Determine the length of an already converted producer.
  [producer-length (-> any/c nonnegative-integer?)]
  
  ;; Determine the unedited length of an already converted producer.
  ;;   In and Out cut points are ignored
  [producer-length/unedited (-> any/c nonnegative-integer?)]

  ;; Get the durration of a clip length in a playslit
  [playlist-clip-length (-> any/c nonnegative-integer? nonnegative-integer?)]
  
  ;; Get the start time of a clip in a playlist
  [playlist-clip-start (-> any/c nonnegative-integer? nonnegative-integer?)]

  ;; Append two playlists together
  ;; NOTE, any properties are currently dropped
  [playlist-append (-> playlist? playlist? playlist?)]))

(define (producer-length producer)
  (define internal (convert producer))
  (define out (mlt-producer-get-out internal))
  (define in (mlt-producer-get-in internal))
  (- out in))

(define (producer-length/unedited producer)
  (mlt-producer-get-length (convert producer)))

(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (convert playlist)))

(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (convert playlist)))

(define (playlist-append pl1 pl2)
  (make-playlist #:elements (append (playlist-elements pl1)
                                    (playlist-elements pl2))))
