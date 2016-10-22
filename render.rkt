#lang racket/base

(provide render
         play
         convert-to-mlt!)
(require racket/match
         racket/dict
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

;; Tests to see if v is a thunk
;; Any -> Boolean
(define (thunk? v)
  (and (procedure? v)
       (procedure-arity-includes? v 0)))

;; Calls mlt-*-service on the correct data type
;;    (getting the service type)
;; Service -> _mlt-service
(define (mlt-*-service video-object)
  (cond
    [(link? video-object)
     (mlt-*-service (link-target video-object))]
    [(filter? video-object)
     (mlt-filter-service (video-mlt-object video-object))]
    [(producer? video-object)
     (mlt-producer-service (video-mlt-object video-object))]
    [else (error 'video "Unsupported video: ~a" video-object)]))

;; Runs a think to its value, if resulting value
;;   is also a thunk it is also applied until a non-thunk
;;   value is returned
;; Any -> Any
(define (run-to-value v)
  (cond
    [(thunk? v) (run-to-value (v))]
    [else v]))

;; Connect target to source
;; Video-Object _mlt-service Integer -> Void
(define (mlt-*-connect target source-service [index #f])
  (cond
    [(consumer? target)
     (mlt-consumer-connect (video-mlt-object target)
                           source-service)]
    [(filter? target)
     (mlt-filter-connect (video-mlt-object target)
                         source-service
                         index)]
    [else (error 'video "Unsupported target ~a" target)]))

;; Convert a video object into an MLT object
;; Video -> MLT-Object
(define (convert-to-mlt! data [profile #f])
  (define p (or profile (mlt-profile-init #f)))
  (define current-tractor (make-parameter #f))
  (let loop ([data data])
    (let/ec continue

      ;; Return early if data has already been computed
      (when (video-mlt-object data)
        (continue (video-mlt-object data)))

      ;; Process Data
      (define ret
        (match data
          [(struct* link ([source source]
                          [target target]
                          [index index]))
           (loop source)
           (define target* (loop target))
           (mlt-*-connect target (mlt-*-service source) index)
           target*]
          [(struct* consumer ([type type]
                              [target target]))
           (mlt-factory-consumer p type target)]
          [(struct* filter ([type type]
                            [source source]))
           (mlt-factory-filter p type source)]
          [(struct* playlist ([elements elements]))
           (define playlist* (mlt-playlist-init))
           (for ([i (in-list elements)])
             (define i* (loop i))
             (match i
               [(struct* playlist-producer ([start start]
                                            [end end]))
                #:when (and start end)
                (mlt-playlist-append-io playlist* i* start end)]
               [(struct* producer ())
                (mlt-playlist-append playlist* i*)]
               [(struct* transition ()) (void)] ;; Must be handled after clips are added
               [(struct* blank ([length length]))
                (mlt-playlist-blank playlist* (run-to-value length))]
               [_ (error 'playlist "Not a playlist element: ~a" i)]))
           (for ([e (in-list elements)]
                 [i (in-naturals)])
             (when (transition? e)
               (mlt-playlist-mix playlist*
                                 (- i 1)
                                 (transition-length e)
                                 (video-mlt-object e))))
           playlist*]
          [(struct* playlist-producer ([producer producer]))
           (loop producer)]
          [(struct* blank ())
           #f] ;; Blanks don't have an MLT object
          [(struct* transition ([type type]
                                [source source]))
           (mlt-factory-transition p type source)]
          [(struct* tractor ([multitrack multitrack]
                             [field field]))
           (define tractor* (mlt-tractor-new))
           (parameterize ([current-tractor tractor*])
             (loop multitrack)
             (loop field)
             (mlt-tractor-producer tractor*))]
          [(struct* multitrack ([tracks tracks]))
           (define t (current-tractor))
           (define multitrack* (mlt-tractor-multitrack t))
           (for ([track (in-list tracks)]
                 [i (in-naturals)])
             (define track* (loop track))
             (mlt-multitrack-connect multitrack* track* i))
           multitrack*]
          [(struct* field ([field-elements field-elements]))
           (define t (current-tractor))
           (define field* (mlt-tractor-field t))
           (for ([element (in-list field-elements)])
             (match element
               [(struct* field-element ([element element]
                                        [track track]
                                        [track-2 track-2]))
                (define element* (loop element))
                (define track* (run-to-value track))
                (define track-2* (run-to-value track-2))
                (cond
                  [(transition? element)
                   (mlt-field-plant-transition field* element* track* track-2*)]
                  [(filter? element)
                   (mlt-field-plant-filter field* element* track*)])]))
           field*]
          [(struct* producer ([source source]
                              [type type]
                              [start start]
                              [end end]))
           (define producer* (mlt-factory-producer p type source))
           (when (and start end)
             (mlt-producer-set-in-and-out producer* start end))
           producer*]
          [_ (error 'video "Unsuported data ~a" data)]))
      (when (video? data)
        (set-video-mlt-object! data ret))
      
      ;; Set properties
      (when (properties? data)
        (for ([(k v-raw) (in-dict (properties-prop data))])
          (define v (run-to-value v-raw))
          (cond
            [(integer? v) (mlt-properties-set-int64 ret k v)]
            [(real? v) (mlt-properties-set-double ret k v)]
            [(string? v) (mlt-properties-set ret k v)]
            [(boolean? v) (mlt-properties-set/bool k v)]
            [(anim-property? v)
             (match v
               [(struct* anim-property ([value value]
                                        [position position]
                                        [length length]))
                (cond
                  [(string? value)
                   (mlt-properties-anim-set ret value position length)]
                  [else (error 'video "Anim Property type ~a not currently supported" value)])])]
            [else (error 'video "Property type ~a not currently supported" v)])))
     
      ;; Attach filters
      (when (service? data)
        (for ([f (in-list (service-filters data))])
          (mlt-service-attach (video-mlt-object data) (loop f))))
      
      ;; Optimise if possible
      (when (producer? ret)
        (mlt-producer-optimise (video-mlt-object ret)))
      
      ret)))

;; Render a video object (including the links
;; Video (#:profile string #:timeout Number) -> Void
(define (render data
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #;fps [fps 25]
                #:timeout [timeout #f])

  (define fps* (rationalize (inexact->exact fps) 1/1000000))
  (define p (mlt-profile-init profile-name))
  (set-mlt-profile-width! p width)
  (set-mlt-profile-height! p height)
  (set-mlt-profile-frame-rate-den! p (denominator fps*))
  (set-mlt-profile-frame-rate-num! p (numerator fps*))
  (define target (convert-to-mlt! data p))
  (play target #:timeout timeout))

;; Play a video that has already been converted to an MLT object
;; _mlt_properties (#:timeout Number) -> Void
(define (play target
              #:timeout [timeout #f])
  (mlt-consumer-start target)
  (let loop ([timeout timeout])
    (sleep 1)
    (when (and timeout (zero? timeout))
      (mlt-consumer-stop target))
    (unless (mlt-consumer-is-stopped target)
      (loop (and timeout (sub1 timeout))))))
