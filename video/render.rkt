#lang racket/base

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

(require racket/contract/base
         racket/match
         racket/math
         racket/dict
         racket/class
         racket/file
         (except-in ffi/unsafe ->)
         (prefix-in file: file/convertible)
         (only-in pict pict? pict->bitmap)
         "private/init-mlt.rkt"
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt"
         "private/utils.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide
 (contract-out
  [rendering? (-> any/c boolean?)]
  [get-rendering-length (-> rendering? (or/c nonnegative-integer? #f))]
  [get-rendering-position (-> rendering? (or/c nonnegative-integer? #f))]
  
  ;; Render a video object (including the links
  [render (->* [any/c]
               [(or/c path-string? path? #f)
                #:dest-filename (or/c path-string? path? #f)
                #:render-mixin (-> class? class?)
                #:profile-name (or/c string? #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps number?
                #:start (or/c nonnegative-integer? #f)
                #:end (or/c nonnegative-integer? #f)
                #:speed (or/c number? #f)
                #:timeout (or/c number? #f)
                #:rendering-box (or/c box? #f)]
               void?)])
 render%
 render<%>)

(define (render video
                [dest #f]
                #:dest-filename [dest-filename #f]
                #:render-mixin [render-mixin values]
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #:start [start #f]
                #:end [end #f]
                #:fps [fps 25]
                #:speed [speed #f]
                #:timeout [timeout #f]
                #:rendering-box [rendering-box #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define r% (render-mixin render%))
  (define renderer
    (new r%
         [dest-dir dest*]
         [dest-filename dest-filename]
         [width width]
         [height height]
         [fps fps]))
  (let* ([res (send renderer setup-profile)]
         [res (send renderer prepare video)]
         [target (send renderer render res)]
         [_ (when (box? rendering-box)
              (set-box! rendering-box (rendering res)))]
         [res (send renderer play res target start end speed timeout)])
    (void)))

(define render<%>
  (interface () get-fps get-profile setup-profile prepare render play))

(define render%
  (class* object% (render<%>)
    (super-new)
    (init-field dest-dir
                [dest-filename #f]
                [prof-name #f]
                [width 720]
                [height 576]
                [fps 25])
    
    (define res-counter 0)
    (define profile (mlt-profile-init prof-name))
    
    (define/private (get-current-filename)
      (begin0 (format "resource~a" res-counter)
              (set! res-counter (add1 res-counter))))

    (define/public (get-fps)
      fps)

    (define/public (get-profile)
      profile)

    (define/public (setup-profile)
      (define fps* (rationalize (inexact->exact fps) 1/1000000))
      (set-mlt-profile-width! profile width)
      (set-mlt-profile-height! profile height)
      (set-mlt-profile-frame-rate-den! profile (denominator fps*))
      (set-mlt-profile-frame-rate-num! profile (numerator fps*)))
    
    (define/public (prepare source)
      (parameterize ([current-renderer this]
                     [current-profile profile])
        (cond
          [(pict? source)
           (define pict-name
             (build-path (or dest-dir
                             (make-temporary-file "rktvid~a" 'directory))
                         (get-current-filename)))
           (send (pict->bitmap source) save-file pict-name 'png 100)
           (prepare (make-producer #:source (format "pixbuf:~a" pict-name)))]
          [(file:convertible? source)
           (define ret (or (file:convert source 'mlt)
                           (file:convert source 'video)))
           (or ret (error "Not convertible to video data"))]
          [else (raise-user-error 'render "~a is not convertible" source)])))

    (define/public (render source)
      (parameterize ([current-renderer this])
        (mlt-*-connect (make-consumer) source)))

    (define/public (play source target start end speed timeout)
      (mlt-producer-set-in-and-out source (or start -1) (or end -1))
      (when speed
        (mlt-producer-set-speed source (exact->inexact speed)))
      (mlt-consumer-start target)
      (let loop ([timeout timeout])
        (sleep 1)
        (when (and timeout (zero? timeout))
          (mlt-consumer-stop target))
        (unless (mlt-consumer-is-stopped target)
          (loop (and timeout (sub1 timeout))))))))

(struct rendering (obj))

(define (get-rendering-length source)
  (mlt-producer-get-length (rendering-obj source)))

(define (get-rendering-position source)
  (mlt-producer-position (rendering-obj source)))

;; Set the current renderer
(when (ffi-lib? mlt-lib)
  (let ([r (new render% [dest-dir #f])])
    (send r setup-profile)
    (current-renderer r)
    (current-profile (send r get-profile))))
