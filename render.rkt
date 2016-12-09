#lang racket/base

(require racket/contract/base
         racket/match
         racket/dict
         racket/class
         file/convertible
         "private/init-mlt.rkt"
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide
 (contract-out
  ;; Render a video object (including the links
  [render (->* [convertible?]
               [(or/c path-string? path?)
                #:render-mixin (-> class? class?)
                #:profile-name (or/c string? #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps number?
                #:timeout (or/c number? #f)]
               void?)])
 render%
 render<%>)

(define (render video
                [dest (current-directory)]
                #:render-mixin [render-mixin values]
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #:fps [fps 25]
                #:timeout [timeout #f])
  (define r% (render-mixin render%))
  (define renderer
    (new r%
         [dest-dir dest]
         [width width]
         [height height]
         [fps fps]))
  (let* ([res (send renderer prepare video)]
         [res (send renderer render res dest)]
         [res (send renderer play res timeout)])
    (void)))

(define render<%>
  (interface () prepare render play))

(define render%
  (class* object% (render<%>)
    (super-new)
    (init-field dest-dir
                [width 720]
                [height 576]
                [fps 25])
    
    (define/public (prepare source)
      (define fps* (rationalize (inexact->exact fps) 1/1000000))
      (set-mlt-profile-width! profile width)
      (set-mlt-profile-height! profile height)
      (set-mlt-profile-frame-rate-den! profile (denominator fps*))
      (set-mlt-profile-frame-rate-num! profile (numerator fps*))
      (cond
        [(convertible? source)
         (define ret (convert source 'mlt))
         (or ret (error "Not convertible to video data"))]
        [else (raise-user-error 'render "~a is not convertible" source)]))

    (define/public (render source dest)
      (mlt-*-connect (make-consumer) source))
    
    (define/public (play target timeout)
      (mlt-consumer-start target)
      (let loop ([timeout timeout])
        (sleep 1)
        (when (and timeout (zero? timeout))
          (mlt-consumer-stop target))
        (unless (mlt-consumer-is-stopped target)
          (loop (and timeout (sub1 timeout))))))))
