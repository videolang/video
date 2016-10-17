#lang racket

;; This file contains a lot of examples. Nominally these should be check-equal?
;; unit tests, but I am not yet sure how best to do that with videos

(require "../main.rkt"
         "video.rkt")

(define demo "/Users/leif/demo.mkv")
(define logo "/Users/leif/logo.png")
(define hold:logo "hold:/Users/leif/logo.png")
(define water "/Users/leif/water.txt")

#;
(render
 (make-link #:source (make-producer #:source demo)
            #:target (make-consumer)))

#;
(render
 (make-link
  #:source (make-link
            #:source (make-producer #:source "/Users/leif/demo.mkv")
            #:target (make-filter #:type 'grayscale)
            #:index 0)
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source (make-playlist
            #:producers (list
                         (make-producer
                          #:source "/Users/leif/demo.mkv"
                          #:start 0
                          #:end 100)
                         (make-producer
                          #:source "/Users/leif/demo.mkv"
                          #:filters (list (make-filter #:type 'grayscale))
                          #:start 100
                          #:end 200)))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source (make-transition
            #:playlist (make-playlist
                        #:producers (list
                                     (make-producer
                                      #:source "/Users/leif/demo.mkv"
                                      #:start 0
                                      #:end 300)
                                     (make-producer
                                      #:source "/Users/leif/demo.mkv"
                                      #:filters (list (make-filter #:type 'grayscale))
                                      #:start 200
                                      #:end 600)))
            #:type 'luma
            #:index 0
            #:length 100)
  ;#:target (make-consumer #:type 'avformat #:target "output.mp4")))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-transition
   #:playlist (make-transition
               #:playlist (make-playlist
                           #:producers (list
                                        (make-playlist-producer
                                         #:producer (make-producer #:source "/Users/leif/demo.mkv")
                                         #:start 0
                                         #:end 300)
                                        (make-playlist-producer
                                         #:producer
                                         (make-producer #:source "/Users/leif/demo.mkv"
                                                        #:filters (list
                                                                   (make-filter #:type 'grayscale)))
                                         #:start 200
                                         #:end 600)
                                        (make-playlist-producer
                                         #:producer
                                         (make-producer
                                          #:source "/Users/leif/demo.mkv"
                                          #:filters (list (make-filter #:type 'invert)))
                                         #:start 500
                                         #:end 900)))
               #:type 'luma
               #:index 1
               #:length 100)
   #:type 'luma
   #:index 0
   #:length 100)
  ;#:target (make-consumer #:type 'avformat #:target "output.mp4")))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source (make-playlist
            #:producers (list (make-producer #:type 'color #:source "green" #:start 0 #:end 200)
                              (make-producer #:type 'color #:source "blue" #:start 0 #:end 200)))
  #:target (make-consumer #:type 'avformat #:target "output.mp4")))
  ;#:target (make-consumer)))


#;
(render
 (make-link
  #:source (make-producer #:type 'color #:source "green" #:start 0 #:end 800)
  ;#:target (make-consumer #:type 'avformat #:target "output.mp4")))
  #:target (make-consumer)))

#;
(render
 (make-link #:source (make-producer #:source demo)
            #:target (make-consumer #:prop (hash "width" (* 16 80)
                                                 "height" (* 10 80)))))

#;
(render
 (make-link #:source (make-producer #:source "/Users/leif/demo.mkv")
            #:target (make-consumer #:type 'avformat
                                    #:target "output.mp4"
                                    #:prop (hash "width" (* 16 80)
                                                 "height" (* 9 80)))))

#;
(render
 (make-link
  #:source (make-playlist
            #:producers (list (make-producer #:type #f #:source "hold:/Users/leif/logo.png"
                                             #:start 0 #:end 200)
                              (make-producer #:type #f #:source "color:blue" #:start 0 #:end 200)))
  #:target (make-consumer #:type 'avformat #:target "output.mp4")))
  ;#:target (make-consumer)))
  ;#:target (make-consumer #:type 'xml #:target "output.xml")))

#;
(render
 (make-link
  #:source
  (make-tractor
   #:multitrack (make-multitrack #:tracks (list (make-producer #:source demo
                                                               #:prop (hash "width" 100
                                                                            "height" 100))
                                                (make-producer #:source demo
                                                               #:prop (hash "width" 200
                                                                            "height" 200)))))
  #:target (make-consumer #:type 'xml)))

#;
(render
 (make-link #:source (make-producer
                      #:source demo
                      #:filters (list
                                 (make-filter #:type 'watermark #:source water)))
            #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-tractor
   #:multitrack (make-multitrack #:tracks (list (make-producer #:source demo)
                                                (make-producer #:source "pango:/Users/leif/water.txt"
                                                               #:prop (hash "in" 0
                                                                            "out" 200
                                                                            "length" 200
                                                                            "a_track" 0
                                                                            "b_track" 1)))))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-tractor
   #:multitrack (make-multitrack
                 #:tracks (list (make-producer #:source demo)
                                (make-producer #:type 'hold #:source logo
                                               #:prop (hash "in" 0
                                                            "out" 200
                                                            "length" 200
                                                            "a_track" 0
                                                            "b_track" 1)))))
  #:target (make-consumer)))

(render
 (make-link
  #:source
  (make-tractor
   #:multitrack
   (make-multitrack
    #:tracks (list (make-producer #:source demo)
                   (make-producer #:source "pango:/Users/leif/water.txt"
                                  #:prop (hash "in" 0
                                               "out" 200
                                               "length" 200
                                               "a_track" 0
                                               "b_track" 1))))
   #:field (make-field
            #:field-elements
            (list (make-field-element
                   #:element (make-transition #:type 'composit
                                              #:source "10%/10%:15%x15%")
                   #:track 0
                   #:track-2 1))))
  #:target (make-consumer)))
