#lang racket

;; This file contains a lot of examples. Nominally these should be check-equal?
;; unit tests, but I am not yet sure how best to do that with videos

;; Uncomment the test you wish to use

(require "../render.rkt"
         "video.rkt"
         "../player.rkt")

;; Some constants that rely on files in my hard drive
;; change them for your own tests
(define demo "/Users/leif/demo.mkv")
(define logo "/Users/leif/logo.png")
(define hold:logo "hold:/Users/leif/logo.png")
(define pixbuf:logo "pixbuf:/Users/leif/logo.png")
(define water "/Users/leif/water.txt")
(define pdf "/Users/leif/demo.pdf")

;; A simple HELLO world MLT program that renders the demo file
#;
(render
 (make-link #:source (make-producer #:source demo)
            #:target (make-consumer)))

;; Simple example of a more complex setup, with a video
;;  Passed into a grayscale filter, and then played
#;
(render
 (make-link
  #:source (make-link
            #:source (make-producer #:source demo)
            #:target (make-filter #:type 'grayscale)
            #:index 0)
  #:target (make-consumer)))

;; Now the filter is attached to the clip.
;;   Also, introduce playlists.
;;   Should start colored and then turn to grayscale
#;
(render
 (make-link
  #:source (make-playlist
            #:elements (list
                        (make-producer
                         #:source demo
                         #:start 0
                         #:end 100)
                        (make-producer
                         #:source demo
                         #:filters (list (make-filter #:type 'grayscale))
                         #:start 100
                         #:end 200)))
  #:target (make-consumer)))

;; Another playlist demo, this time using 3 filters
#;
(render
 (make-link
  #:source
  (make-playlist
   #:elements (list
               (make-playlist-producer
                #:producer (make-producer #:source "/Users/leif/demo.mkv")
                #:start 0
                #:end 300)
               (make-transition
                #:type 'luma
                #:length 100)
               (make-playlist-producer
                #:producer
                (make-producer #:source demo
                               #:filters (list
                                          (make-filter #:type 'grayscale)))
                #:start 200
                #:end 600)
               (make-transition
                #:type 'luma
                #:length 100)
               (make-playlist-producer
                #:producer
                (make-producer
                 #:source demo
                 #:filters (list (make-filter #:type 'invert)))
                #:start 500
                #:end 900)))
  #:target (make-consumer)))

; Draw a green and then blue square to a file output.mp4
#;
(render
 (make-link
  #:source (make-playlist
            #:producers (list (make-producer #:type 'color #:source "green" #:start 0 #:end 200)
                              (make-producer #:type 'color #:source "blue" #:start 0 #:end 200)))
  #:target (make-consumer #:type 'avformat #:target "output.mp4")))

;; The demo video now scaled to a 16:10 ratio
#;
(render
 (make-link #:source (make-producer #:source demo)
            #:target (make-consumer #:prop (hash "width" (* 16 80)
                                                 "height" (* 10 80)))))

;; Same as above, but now renders to output.mp4
#;
(render
 (make-link #:source (make-producer #:source demo
            #:target (make-consumer #:type 'avformat
                                    #:target "output.mp4"
                                    #:prop (hash "width" (* 16 80)
                                                 "height" (* 9 80))))))

;; Displays a logo, and then shows a blue square
;;   Also, render to XML
#;
(render
 (make-link
  #:source (make-playlist
            #:producers (list (make-producer #:type #f #:source hold:logo
                                             #:start 0 #:end 200)
                              (make-producer #:type #f #:source "color:blue" #:start 0 #:end 200)))
  #:target (make-consumer #:type 'xml #:target "output.xml")))

;; Display a watermark over demo
#;
(render
 (make-link #:source (make-producer
                      #:source demo
                      #:filters (list
                                 (make-filter #:type 'watermark #:source water)))
            #:target (make-consumer)))

;; Everything below this is currently scratch and should be cleaned up eventually!
;; Everything below this is currently scratch and should be cleaned up eventually!
;; Everything below this is currently scratch and should be cleaned up eventually!
;; Everything below this is currently scratch and should be cleaned up eventually!


#;
(render
 (make-link
  #:source
  (make-multitrack #:tracks (list (make-producer #:source demo)
                                  (make-producer #:source "pango:/Users/leif/water.txt"
                                                 #:prop (hash "in" 0
                                                              "out" 200
                                                              "length" 200
                                                              "a_track" 0
                                                              "b_track" 1))))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-multitrack
   #:tracks (list (make-producer #:source demo)
                  (make-producer #:type 'hold #:source logo
                                 #:prop (hash "in" 0
                                              "out" 200
                                              "length" 200
                                              "a_track" 0
                                              "b_track" 1))))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-multitrack
   #:tracks (list (make-producer #:source demo)
                  (make-producer #:source hold:logo
                                 #:prop (hash "in" 0
                                              "out" 300)))
   #:field (list (make-field-element
                  #:element (make-transition #:type 'composite
                                             #:source "10%/10%:15%x15%")
                  #:track 0
                  #:track-2 1)))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-multitrack
   #:tracks (list (make-producer #:source demo)
                  (make-producer #:source "pixbuf:/Users/leif/logo.png"
                                 #:prop (hash "in" 0
                                              "out" 300
                                              )))
   #:field (list (make-field-element
                  #:element (make-transition #:type 'composite
                                             #:source "10%/10%:15%x15%")
                  #:track 0
                  #:track-2 1)))
  #:target (make-consumer)))

#;
(render
 (make-link
  #:source
  (make-tractor
   #:multitrack
   (make-multitrack
    #:tracks (list (make-producer #:source demo #:tag 'demo-reel)
                   (make-producer #:source "pixbuf:/Users/leif/logo.png"
                                  #:prop (hash "in" 0
                                               "out" (λ ()
                                                       (floor
                                                        (/ (properties-ref (find-tag 'demo-reel)
                                                                           "length"
                                                                           'mlt-position)
                                                           10)))))))
   #:field (make-field
            #:field-elements
            (list (make-field-element
                   #:element (make-transition #:type 'composite
                                              #:source "10%/10%:15%x15%")
                   #:track 0
                   #:track-2 1))))
  #:target (make-consumer)))


#;
(render
 (make-link
  #:source (make-playlist
            #:elements
            (list (make-tractor #:multitrack
                                (make-multitrack
                                 #:tracks
                                 (list
                                  (make-playlist
                                   #:elements (list (make-producer #:source demo))))))))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source (make-tractor
            #:multitrack (make-multitrack
                          #:tracks
                          (list
                           (make-tractor
                            #:multitrack (make-multitrack
                                          #:tracks
                                          (list (make-producer #:source demo
                                                               #:prop (hash "out" 200))
                                                (make-playlist
                                                 #:elements (list
                                                             (make-blank #:length 200)
                                                             (make-producer
                                                              #:source demo
                                                              #:prop (hash "in" 300))))))))))
  ;#:target (make-consumer)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source
  (make-tractor
   #:multitrack
   (make-multitrack
    #:tracks (list (make-producer #:source demo #:tag 'demo-reel)
                   (make-producer #:source demo
                                  #:prop (hash "in" 200
                                               "out" 600))))
   #:field (make-field
            #:field-elements
            (list (make-field-element
                   #:element (make-transition #:type 'composite
                                              #:source "10%/10%:15%x15%")
                   #:track 0
                   #:track-2 1))))
  #:target (make-consumer)))


#;
(render
 (make-link
  #:source
  (make-playlist
   #:elements
   (list (make-playlist-producer
          #:producer (make-playlist
                      #:elements (list
                                  (make-playlist-producer
                                   #:producer (make-producer #:source demo)
                                   #:start 0
                                   #:end 300)))
          #:start 0
          #:end 300)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source
  (make-playlist
   #:elements
   (list (make-playlist-producer
          #:producer (make-producer #:source demo)
          #:start 0
          #:end 300)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source
  (make-playlist
   #:elements
   (list
    (make-tractor
     #:multitrack (make-multitrack
                   #:tracks
                   (list
                    (make-tractor
                     #:multitrack (make-multitrack
                                   #:tracks
                                   (list (make-producer #:source demo
                                                        #:prop (hash "out" 200))
                                         (make-playlist
                                          #:elements (list
                                                      (make-blank #:length 200)
                                                      (make-producer
                                                       #:source demo
                                                       #:prop (hash "in" 300))))))))))))
  ;#:target (make-consumer)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))


#;
(render
 (make-link
  #:source
  (make-playlist #:elements (list (make-producer #:source demo)
                                  (make-producer #:source demo)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source
  (make-playlist
   #:elements (list (make-tractor
                     #:multitrack
                     (make-multitrack
                      #:tracks
                      (list (make-producer #:source demo))))
                    (make-producer #:source demo)))
  ;#:target (make-consumer)))
  #:target (make-consumer #:type 'xml #:target "out.xml")))

#;
(render
 (make-link
  #:source
  (make-playlist #:elements (list (make-playlist #:elements (list (make-producer #:source demo)))))
  #:target (make-consumer #:type 'xml #:target "out.xml")))


#;
(render
 (make-link
  #:source (make-producer #:source "/Users/leif/demo.pdf")
  #:target (make-consumer)))

#;
(define vp
  (new video-player%
         [video (make-producer #:source demo)]))

#|
(send vp show #t)
(send vp play)
(send vp set-video (make-producer #:source demo))
|#

#;
(render
 (make-link #:source (make-producer #:source demo
                                    #:filters (list (make-filter #:type 'avfilter.afade
                                                                 #:source "in"
                                                                 #:prop (hash "av.duration" 100))))
            #:target (make-consumer)))

#;
(render
 (make-link #:source (make-producer #:type 'loader
                                    #:source "pango"
                                    #:prop (hash "text" "Hello World"))
            #:target (make-consumer)))
