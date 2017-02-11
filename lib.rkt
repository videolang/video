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
  [producer-length (-> producer? nonnegative-integer?)]
  
  ;; Determine the unedited length of an already converted producer.
  ;;   In and Out cut points are ignored
  [producer-length/unedited (-> producer? nonnegative-integer?)]

  ;; Get the durration of a clip length in a playslit
  [playlist-clip-length (-> playlist? nonnegative-integer? nonnegative-integer?)]
  
  ;; Get the start time of a clip in a playlist
  [playlist-clip-start (-> playlist? nonnegative-integer? nonnegative-integer?)]))

(define (producer-length producer)
  (define internal (convert producer
                            #:renderer (make-renderer producer)))
  (define out (mlt-producer-get-out internal))
  (define in (mlt-producer-get-in internal))
  (- out in))

(define (producer-length/unedited producer)
  (mlt-producer-get-length (convert producer #:renderer (make-renderer producer))))

(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (convert playlist #:renderer (make-renderer playlist)) index))

(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (convert playlist #:renderer (make-renderer playlist)) index))

;; Use existing renderer or produce new one
;; Producer -> Renderer
(define (make-renderer producer)
  (new render% [video producer]))
