#lang racket/base

(provide render)
(require racket/match
         racket/dict
         "private/mlt.rkt"
         "private/video.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

;; Calls mlt-*-service on the correct data type
;;    (getting the service type)
;; Service -> _mlt-service
(define (mlt-*-service video-object)
  (cond
    [(link? video-object)
     (mlt-*-service (link-target video-object))]
    [(filter? video-object)
     (mlt-filter-service (video-mlt-object video-object))]
    [(transition? video-object)
     (mlt-*-service (transition-playlist video-object))]
    [(producer? video-object)
     (mlt-producer-service (video-mlt-object video-object))]
    [else (error 'video "Unsupported video: ~a" video-object)]))

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

;; Append a clip to the appropriate playlist
;; _mlt-playlist Producer -> Void
(define (playlist-append playlist pro)
  (match pro
    [(struct* playlist-producer ([start start]
                                 [end end]))
     #:when (and start end)
     (mlt-playlist-append-io playlist (video-mlt-object pro) start end)]
    [else
     (mlt-playlist-append playlist (video-mlt-object pro))]))

(define current-profile (make-parameter #f))
(define current-tractor (make-parameter #f))
(define optimise-playlists? (make-parameter #t))
;; Convert a video object into an MLT object
;; Video -> MLT-Object
(define (convert-to-mlt! data)
  (define p (current-profile))

  ;; Process Data
  (define ret
    (match data
      [(struct* link ([source source]
                      [target target]
                      [index index]))
       (convert-to-mlt! source)
       (define target* (convert-to-mlt! target))
       (mlt-*-connect target (mlt-*-service source) index)
       target*]
      [(struct* consumer ([type type]
                          [target target]))
       (mlt-factory-consumer p type target)]
      [(struct* filter ([type type]))
       (mlt-factory-filter p type #f)] ;; TODO, should probably not be #f?
      [(struct* playlist ([producers producers]))
       (define playlist (mlt-playlist-init))
       (for ([i (in-list producers)])
         (parameterize ([optimise-playlists? #t])
           (convert-to-mlt! i))
         (playlist-append playlist i))
       playlist]
      [(struct* playlist-producer ([producer producer]))
       (convert-to-mlt! producer)]
      [(struct* transition ([type type]
                            [playlist playlist]
                            [index index]
                            [length length]))
       (define opt? (optimise-playlists?))
       (parameterize ([optimise-playlists? #f])
         (define playlist* (convert-to-mlt! playlist))
         (define transition* (mlt-factory-transition p type #f)) ; TODO, should probably not be #f?
         (mlt-playlist-mix playlist* index length transition*)
         #;(when opt?
           (mlt-producer-optimise playlist*))
         playlist*)]
      [(struct* tractor ([multitrack multitrack]
                         [field field]))
       (define tractor* (mlt-tractor-new))
       (parameterize ([current-tractor tractor*])
         (convert-to-mlt! multitrack)
         (convert-to-mlt! field)
         (mlt-tractor-producer tractor*))]
      [(struct* multitrack ([tracks tracks]))
       (define t (current-tractor))
       (define multitrack* (mlt-tractor-multitrack t))
       (for ([track (in-list tracks)]
             [i (in-naturals)])
         (define track* (convert-to-mlt! track))
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
            (define element* (convert-to-mlt! element))
            (cond
              [(transition? element)
               (mlt-field-plant-transition element* track track-2)]
              [(filter? element)
               (mlt-field-plant-filter element* track)])]))
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
    (for ([(k v) (in-dict (properties-prop data))])
      (cond
        [(integer? v) (mlt-properties-set-int64 ret k v)]
        [(real? v) (mlt-properties-set-double ret k v)]
        [(string? v) (mlt-properties-set ret k v)]
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
      (mlt-service-attach (video-mlt-object data) (convert-to-mlt! f))))

  ret)

(define (render data)
  
  (mlt-factory-init #f)
  (define p (mlt-profile-init #f))
  
  (define target (parameterize ([current-profile p]
                                [optimise-playlists? #t])
                   (convert-to-mlt! data)))

  (mlt-consumer-start target)
 
  (let loop ()
    (sleep 1)
    (unless (mlt-consumer-is-stopped target)
      (loop))))
