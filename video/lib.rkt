#lang racket/base

(require racket/dict
         racket/class
         racket/contract/base
         racket/port
         racket/math
         "private/video.rkt"
         "private/mlt.rkt"
         "private/utils.rkt"
         "render.rkt"
         (prefix-in list: "render/list.rkt")
         (for-syntax racket/base
                     syntax/parse))

(provide
 (contract-out
  ;; Determine the unedited length of an already converted producer.
  ;;   In and Out cut points are ignored
  [producer-length/unedited (-> any/c nonnegative-integer?)]

  ;; Get the durration of a clip length in a playslit
  [playlist-clip-length (-> any/c nonnegative-integer? nonnegative-integer?)]
  
  ;; Get the start time of a clip in a playlist
  [playlist-clip-start (-> any/c nonnegative-integer? nonnegative-integer?)]

  ;; Get the number of clips in a playlist
  [playlist-clip-count (-> any/c nonnegative-integer?)])

 ;; PROTOTYPE FUNCTION, SHOULD BE MOVED SOMEWHERE ELSE
 avformat-installed-codecs)

(define (producer-length/unedited producer)
  (mlt-producer-get-length (convert producer)))

(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (convert playlist) index))

(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (convert playlist) index))

(define (playlist-clip-count playlist)
  (mlt-playlist-count (convert playlist)))

(define (avformat-installed-codecs)
  (render (make-producer #:type 'color #:source "black")
          #:render-mixin list:render-mixin))
