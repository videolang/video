#lang racket/base

(require racket/match
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
    [(producer? video-object)
     (mlt-producer-service (video-mlt-object video-object))]
    [(filter? video-object)
     (mlt-filter-service (video-mlt-object video-object))]
    [(transition? video-object)
     (mlt-*-service (transition-playlist video-object))]
    [else (error 'video "Unsupported video: ~a" video-object)]))

;; Connect target to source
;; Video-Object _mlt-service Integer -> _ibool
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
;; _mlt-playlist Producer -> _ibool
(define (playlist-append playlist pro)
  (match pro
    [(struct* producer ([start start]
                        [end end]))
     #:when (and start end)
     (mlt-playlist-append-io playlist (video-mlt-object producer) start end)]
    [else
     (mlt-playlist-append playlist (video-mlt-object producer))]))

(define profile (make-parameter #f))
(define optimise-playlists? (make-parameter #t))
;; Convert a video object into an MLT object
;; Video -> MLT-Object
(define (convert-to-mlt! data)
  (define p (profile))

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
      [(struct* producer ([source source]
                          [type type]))
       (define type (producer-type data))
       (mlt-factory-producer p type source)]
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
      [(struct* transition ([type type]
                            [playlist playlist]
                            [index index]
                            [length length]))
       (define opt? (optimise-playlists?))
       (parameterize ([optimise-playlists? #f])
         (define playlist* (convert-to-mlt! playlist))
         (define transition* (mlt-factory-transition p type #f)) ; TODO, should probably not be #f?
         (mlt-playlist-mix playlist* index length transition*)
         (when opt?
           (mlt-producer-optimise playlist*))
         playlist*)]
      [_ (error 'video "Unsuported data ~a" data)]))
  (when (video? data)
    (set-video-mlt-object! data ret))

  ;; Attach filters
  (when (service? data)
    (for ([f (in-list (service-filters data))])
      (mlt-service-attach (video-mlt-object data) (convert-to-mlt! f))))
  
  ret)

(define (render data)
  
  (unless (mlt-factory-init #f)
    (error "Unable to locate factory modules"))
  (define p (mlt-profile-init #f))
  
  (define target (parameterize ([profile p]
                                [optimise-playlists? #t])
                   (convert-to-mlt! data)))

  (mlt-consumer-start target)
 
  (let loop ()
    (sleep 1)
    (unless (mlt-consumer-is-stopped target)
      (loop))))

(define demo "/Users/leif/demo.mkv")

(render
 (make-link #:source (make-producer #:source "/Users/leif/demo.mkv")
            #:target (make-consumer)))

#;
(render
 (build-video-object
  'link
  #:source (build-video-object
            'link
            #:source (build-video-object 'clip #:source "/Users/leif/demo.mkv")
            #:target (build-video-object 'filter #:type 'grayscale)
            #:index 0)
  #:target (build-video-object 'consumer)))

#;
(render
 (bvo
  'link
  #:source (bvo 'playlist
                #:producers (list
                             (bvo 'clip
                                  #:source "/Users/leif/demo.mkv"
                                  #:start 0
                                  #:end 100)
                             (bvo 'clip
                                  #:source "/Users/leif/demo.mkv"
                                  #:filters (list (bvo 'filter #:type 'grayscale))
                                  #:start 100
                                  #:end 200)))
  #:target (bvo 'consumer)))

#;
(render
 (bvo
  'link
  #:source (bvo
            'transition
            #:playlist (bvo 'playlist
                            #:producers (list
                                         (bvo 'clip
                                              #:source "/Users/leif/demo.mkv"
                                              #:start 0
                                              #:end 300)
                                         (bvo 'clip
                                              #:source "/Users/leif/demo.mkv"
                                              #:filters (list (bvo 'filter #:type 'grayscale))
                                              #:start 200
                                              #:end 600)))
            #:type 'luma
            #:index 0
            #:length 100)
  #:target (bvo 'consumer #:type 'avformat #:target "output.mp4")))
  ;#:target (bvo 'consumer)))

#;
(render
 (bvo
  'link
  #:source
  (bvo
   'transition
   #:playlist (bvo
               'transition
               #:playlist (bvo 'playlist
                               #:producers (list
                                            (bvo 'clip
                                                  #:source "/Users/leif/demo.mkv"
                                                  #:start 0
                                                  #:end 300)
                                            (bvo 'clip
                                                 #:source "/Users/leif/demo.mkv"
                                                 #:filters (list (bvo 'filter #:type 'grayscale))
                                                 #:start 200
                                                 #:end 600)
                                            (bvo 'clip
                                                 #:source "/Users/leif/demo.mkv"
                                                 #:filters (list (bvo 'filter #:type 'invert))
                                                 #:start 500
                                                 #:end 900)))
               #:type 'luma
               #:index 1
               #:length 100)
   #:type 'luma
   #:index 0
   #:length 100)
  #:target (bvo 'consumer #:type 'avformat #:target "output.mp4")))
  ;#:target (bvo 'consumer)))

#;
(render
 (bvo
  'link
  ;#:source (bvo 'clip #:type 'color #:source "green")
  #:source (bvo 'playlist
                #:producers (list (bvo 'clip #:type 'color #:source "green"))
                #:start 0
                #:end 100)
  #:target (bvo 'consumer #:type 'avformat #:target "output.mp4")))
  ;`#:target (bvo 'consumer)))
