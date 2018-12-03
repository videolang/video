#lang racket/base

#|
   Copyright 2016-2018 Leif Andersen, Stephen Chang

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
         racket/class
         racket/draw
         racket/match
         racket/format
         racket/list
         racket/set
         racket/math
         racket/dict
         racket/hash
         syntax/location
         (except-in pict frame blank)
         graph
         (except-in "private/video.rkt" producer? filter? transition?)
         (prefix-in runtime: "private/ffmpeg-pipeline.rkt")
         "surface.rkt"
         (prefix-in core: "private/video.rkt")
         "units.rkt"
         "private/init.rkt"
         "private/log.rkt"
         (prefix-in ffmpeg: "private/ffmpeg/main.rkt")
         (for-syntax syntax/parse
                     racket/base
                     racket/syntax))

(provide
 (contract-out

  ;; Determine if a transition or merge is a 'field element', which means that
  ;;   it can be placed in the `#:transitions` or `#:merges` keyword.
  [field-element? (-> any/c boolean?)]
  
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (->* []
                   [#:merges (listof field-element?)
                    #:properties (or/c (hash/c string? any/c) #f)
                    #:filters (or/c (listof filter?) #f)]
                   #:rest (listof any/c)
                   producer?)]

  ;; Creates a playlist (tracks playing in sequence)
  ;;   (syntactic sugar for list)
  [playlist (->* []
                 [#:transitions (listof field-element?)
                  #:properties (or/c (hash/c string? any/c) #f)
                  #:filters (or/c (listof filter?) #f)]
                 #:rest (listof any/c)
                 producer?)]

  ;; Creates a blank video, for offsetting
  ;;  clips in a playlist
  [blank (->* () ((or/c nonnegative-integer? #f)) (or/c blank? producer?))]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->producer [path-string?]
                    [#:start number?
                     #:end number?
                     #:length number?])]

  ;; Creates a producer that is a solid color
  ;; TODO: Must be more precise
  ;;   * If argument 2 is provided, argument 3 must be as well
  ;;   * If argument 2 and 3 are provided, argumet one must be a byte
  [color (->producer [(or/c string? (is-a?/c color%)
                            (list/c byte? byte? byte?)
                            byte?)]
                     [(or/c byte? #f)
                      (or/c byte? #f)])]

  ;; Create a producer that is the same as the other producer but with one or more
  ;; filters attached to it
  [attach-filter (-> service? filter? ... producer?)]

  ;; Creates a fading transition from a start to end clip
  [fade-transition (->transition []
                                 [(and/c number? positive?)])]

  ;; Creates a transition where the audio proceeds the video
  [L-cut (->transition []
                       [(and/c number? positive?)])]

  ;; The duel of a L-cut, video proceeds the audio
  [J-cut (->transition []
                       [(and/c number? positive?)])]

  [overlay-merge (->merge [(and/c number? (>=/c 0))
                           (and/c number? (>=/c 0))]
                          [(or/c (and/c number? positive?) #f)
                           (or/c (and/c number? positive?) #f)])]
  
  ;; Creates a composite transition where the top track is
  ;;   placed above the bottom track
  [composite-merge (->merge [(between/c 0 1)
                             (between/c 0 1)
                             (between/c 0 1)
                             (between/c 0 1)]
                            [])]

  ;; Returns the same producer converted to a chapter
  [chapter (-> producer? producer?)]

  #| TODO
  ;; Creates a swiping transition from a start clip to an
  ;;   end clip
  [swipe-transition (->transition [#:direction symbol?]
                                  []
                                  #:direction t/b)]
|#
  
  [scale-filter (-> (and/c real? positive?) (and/c real? positive?) filter?)]
  
  [crop-filter (-> (and/c real? positive?)
                   (and/c real? positive?)
                   (and/c real? positive?)
                   (and/c real? positive?)
                   filter?)]

  [pad-filter (-> (and/c real? positive?)
                  (and/c real? positive?)
                  (and/c real? positive?)
                  (and/c real? positive?)
                  filter?)]

  [transpose-filter (->* []
                         [(or/c 'counter-clock
                              'clock
                              #f)
                          boolean?]
                        filter?)]
  
  [rotate-filter (->* [real?]
                       [#:bilinear? boolean?
                        #:fill-color (or/c string?
                                           (is-a?/c color%)
                                           (list/c byte? byte? byte?))]
                       filter?)]

  [color-channel-mixer-filter (-> (hash/c string? (between/c -2 2)) filter?)]

  [grayscale-filter (-> filter?)]

  [sepia-filter (-> filter?)]
  
  [lowpass-filter (->* []
                       [#:frequency (or/c nonnegative-integer? #f)
                        #:poles (or/c nonnegative-integer? #f)
                        #:width (or/c nonnegative-integer? #f)
                        #:width-type (or/c 'h 'q 'o 's 'k)
                        #:channels #f]
                       filter?)]
  
  [highpass-filter (->* []
                       [#:frequency (or/c nonnegative-integer? #f)
                        #:poles (or/c nonnegative-integer? #f)
                        #:width (or/c nonnegative-integer? #f)
                        #:width-type (or/c 'h 'q 'o 's 'k)
                        #:channels #f]
                       filter?)]

  [compand-filter (->* []
                       [#:attacks (listof real?)
                        #:decays (listof real?)
                        #:points (listof (or/c (cons/c real? real?)
                                               complex?))
                        #:soft-knee (or/c real? #f)
                        #:gain (or/c real? #f)
                        #:volume (or/c real? #f)
                        #:delay (or/c real? #f)]
                       filter?)]

  ;; Filter to remove video from an audio, resulting in a single audio track
  [remove-video (-> filter?)]

  ;; Filter to remove audio from a video track, resulting in a silent video.
  [remove-audio (-> filter?)]

  [mux-filter (-> #:type (or/c 'video 'v 'audio 'a)
                  #:index nonnegative-integer?
                  filter?)]

  ;; Set a property associated with a properties struct
  [set-property (-> (or/c properties? video-convertible?) string? any/c properties?)]

  ;; Get a property associated with a properties struct
  [get-property (->* [properties? string?]
                     [(-> any/c)]
                     any/c)]

  ;; Remove a explicit property associated with a properties sturct.
  [remove-property (-> properties? string? properties?)]

  ;; Generate a new video with a new in-out
  [cut-producer (->* [producer?]
                     [#:start (or/c nonnegative-integer? #f)
                      #:end (or/c nonnegative-integer? #f)]
                     producer?)]

  ;; Envelope filter for audio tracks
  [envelope-filter (->* (#:length nonnegative-integer?
                         #:direction (or/c 'in 'out))
                        (#:curve (or/c 'tri
                                       'qsin
                                       'hsin
                                       'esin
                                       'log
                                       'ipar
                                       'qua
                                       'cub
                                       'squ
                                       'cbr
                                       'par
                                       'exp
                                       'iqsin
                                       'ihsin
                                       'dese
                                       'desi
                                       #f))
                       filter?)])

  external-video
  for/playlist
  for/multitrack)

(define (blank [len #f])
  (color "black"
         #:properties (hash "length" len)))

(define (clip path
              #:start [s #f]
              #:end [e #f]
              #:length [len #f]
              #:filters [filters #f]
              #:properties [properties #f])
  (define clip-path (relative-path->string path))
  (make-file #:path clip-path
             #:prop (hash-union (or properties (hash))
                                (hash "start" s "end" e "length" len)
                                #:combine (λ (prop arg) prop))
             #:filters (or filters '())))

(define-producer (color c [c2 #f] [c3 #f]
                        #:length [length #f])
  #:user-properties prop
  #:properties (λ (r)
                 (let* ([r (if length
                               (hash-union r
                                           (hash "length" length)
                                           #:combine (λ (t s) t))
                               r)]
                        [r (cond
                             [(dict-has-key? r "start") r]
                             [else (dict-set r "start" 0)])]
                        [r (cond
                             [(dict-has-key? r "end") r]
                             [(dict-has-key? r "length") (dict-set r "end" (dict-ref r "length"))]
                             [else (dict-set r "end" +inf.0)])])
                   r))
  #:subgraph (hash 'video
                   (mk-filter "color" (let* ([ret (hash "c" (color->string c c2 c3))]
                                             [ret (if (and length (not (equal? length +inf.0)))
                                                      (hash-set ret "d" length)
                                                      ret)]
                                             [ret (if (and width height)
                                                      (hash-set ret "size" (format "~ax~a"
                                                                                   width
                                                                                   height))
                                                      ret)])
                                        ret))
                   'audio
                   (mk-empty-audio-filter #:duration length))
  (when (or (and c2 (not c3))
            (and (not c2) c3)
            (and c2 c3 (not (byte? c))))
    (error 'color
           "Invalid combination of arguments: c1 ~a; c2 ~a; c3 ~a"
           c c2 c3))
  (define length (or (dict-ref prop "length" #f)
                     (and (dict-ref prop "start" #f)
                          (dict-ref prop "end" #f)
                          (- (dict-ref prop "end") (dict-ref prop "start")))))
  (define width (dict-ref prop "width" #f))
  (define height (dict-ref prop "height" #f)))


;; TODO
;(define-producer (delay prod-to-convert)
;  #:subgraph (λ (prev target-prop target-counts)

(define (multitrack #:merges [transitions '()]
                    #:properties [prop #f]
                    #:filters [maybe-filters #f]
                    . tracks)
  #|
  (for ([t (in-list transitions)])
    (define start (field-element-track t))
    (define end (field-element-track-2 t))
    (unless (or start end)
      (error 'multitrack "Both starting and ending clips are #f"))
    (unless (or (not start) (set-member? tracks start))
      (error 'multitrack "Starting clip ~a not found: ~a" start playlist))
    (unless (or (not end) (set-member? tracks end))
      (error 'multitrack "Ending clip ~a not found: ~a" end playlist)))
  (match-define-values (tracks* transitions* _)
    (for/fold ([tracks* '()]
               [transitions* transitions]
               [prev #f])
              ([track (in-list tracks)]
               [i (in-naturals)])
      (cond
        [(transition? track) (values tracks*
                                     (cons (make-field-element #:element track
                                                               #:track prev
                                                               #:track-2 (list-ref tracks (add1 i)))
                                           transitions*)
                                     prev)]
        [else (values (cons track tracks*) transitions*
                      (if (or (= i 0)
                              (not (transition? (list-ref tracks (sub1 i)))))
                          track
                          prev))])))
|#
  (make-multitrack
   #:tracks (let ()
              (unless (null? transitions)
                (error 'multitrack "Merges field disabled for Video 0.2, use inline merges"))
              tracks)
   #:field '() ;transitions*
   #:prop (or prop (hash))
   #:filters (or maybe-filters '())))

(define (playlist #:transitions [transitions '()]
                  #:properties [prop #f]
                  #:filters [maybe-filters #f]
                  . clips)
  (make-playlist
   #:elements
   (let ()
     (unless (null? transitions)
       (error 'playlist "Transitions field disabled for Video 0.2, use inline transitions"))
     clips)
   #|
   (for/fold ([acc clips])
             ([t (in-list transitions)])
     (define start (field-element-track t))
     (define end (field-element-track-2 t))
     (unless (or start end)
       (error playlist "Both starting and ending clips are #f"))
     (unless (or (not start) (set-member? clips start))
       (error 'playlist "Starting clip ~a not found: ~a" start playlist))
     (unless (or (not end) (set-member? clips end))
       (error 'playlist "Ending clip ~a not found: ~a" end playlist))
     (append*
      (for/list ([clip (in-list clips)])
        (cond
          [(equal? start clip)
           (list clip (field-element-element t))]
          [(and (not start) (equal? end clip))
           (list (field-element-element t) clip)]
          [else (list clip)]))))
|#
   #:prop (or prop (hash))
   #:filters (or maybe-filters '())))

(define (attach-filter obj . f)
  (define new-filters (append f (service-filters obj)))
  (copy-video obj #:filters new-filters))

(define ((LJ-cut delay L/J) #:empty-graph ctx
                            #:track1-props node-a
                            #:track2-props node-b
                            #:target-props target-props
                            #:target-counts target-counts)
  (define len-a (- (dict-ref node-a "end")
                   (dict-ref node-a "start")))
  (define len-b (- (dict-ref node-b "end")
                   (dict-ref node-b "start")))
  (define t-length (- (+ len-a len-b) delay))
  (define in-a
    (mk-filter-node
     (hash 'audio (mk-filter "atrim"
                             (hash "start" 0
                                   "end" (case L/J
                                           [(L) (- len-a delay)]
                                           [(J) len-a])))
           'video (mk-filter "trim"
                             (hash "start" 0
                                   "end" (case L/J
                                           [(L) len-a]
                                           [(J) (- len-a delay)]))))
     #:props node-a
     #:counts target-counts))
  (add-vertex! ctx in-a)
  (define in-b
    (mk-filter-node
     (hash 'video (mk-filter "trim"
                             (hash "start" (case L/J
                                             [(L) delay]
                                             [(J) 0])
                                   "end" len-b))
           'audio (mk-filter "atrim"
                             (hash "start" (case L/J
                                             [(L) 0]
                                             [(J) delay])
                                   "end" len-b)))
     #:props node-b
     #:counts target-counts))
  (add-vertex! ctx in-b)
  (define pts-a
    (mk-filter-node (hash
                     'video (mk-filter "setpts" (hash "expr" "PTS-STARTPTS"))
                     'audio (mk-filter "asetpts" (hash "expr" "PTS-STARTPTS")))
                    #:props (node-props in-a)
                    #:counts (node-counts in-b)))
  (add-vertex! ctx pts-a)
  (add-directed-edge! ctx in-a pts-a 1)
  (define pts-b
    (mk-filter-node (hash
                     'video (mk-filter "setpts"
                                       (hash "expr" "PTS-STARTPTS"))
                     'audio (mk-filter "asetpts" (hash "expr" "PTS-STARTPTS")))
                    #:props (node-props in-b)
                    #:counts (node-counts in-b)))
  (add-vertex! ctx pts-b)
  (add-directed-edge! ctx in-b pts-b 1)
  (define concat
    (mk-filter-node
     (hash 'video (mk-filter "concat"
                             (hash "n" 2
                                   "v" 1
                                   "a" 0))
           'audio (mk-filter "concat"
                             (hash "n" 2
                                   "v" 0
                                   "a" 1)))
     #:props (dict-set* target-props
                        "video-time-base" ffmpeg:AV-TIME-BASE-Q
                        "audio-time-base" ffmpeg:AV-TIME-BASE-Q
                        "start" 0
                        "end" t-length)
     #:counts target-counts))
  (add-vertex! ctx concat)
  (add-directed-edge! ctx pts-a concat 1)
  (add-directed-edge! ctx pts-b concat 2)
  (make-video-subgraph
   #:graph ctx
   #:sources (cons in-a in-b)
   #:sinks concat
   #:prop (hash "length" t-length)))

(define-transition (L-cut [delay 1])
  #:properties (λ (prop)
                 (dict-set* (or prop (hash))
                            "pre-length" delay
                            "post-length" delay))
  #:combined-subgraph (LJ-cut delay 'L))

(define-transition (J-cut [delay 1])
   #:properties (λ (prop)
                 (dict-set* (or prop (hash))
                            "pre-length" delay
                            "post-length" delay))
  #:combined-subgraph (LJ-cut delay 'J))

(define-transition (fade-transition [fade-length 1])
  #:properties (λ (prop)
                 (dict-set* (or prop (hash))
                            "pre-length" fade-length
                            "post-length" fade-length))
  #:combined-subgraph (λ (#:empty-graph ctx
                          #:track1-props node-a
                          #:track2-props node-b
                          #:target-props target-prop
                          #:target-counts target-counts)
                        (define width (or (dict-ref target-prop "width" #f)
                                          (dict-ref node-a "width" #f)
                                          (dict-ref node-b "width" #f)
                                          1920))
                        (define height (or (dict-ref target-prop "height" #f)
                                           (dict-ref node-a "height" #f)
                                           (dict-ref node-b "height" #f)
                                           1080))
                        (define len-a (- (dict-ref node-a "end")
                                         (dict-ref node-a "start")))
                        (define len-b (- (dict-ref node-b "end")
                                         (dict-ref node-b "start")))
                        (define t-length (- (+ len-a len-b) fade-length))
                        (define bg-node
                          (mk-filter-node
                           (hash 'video (mk-filter "color"
                                                   (let* ([r (hash "color" "black"
                                                                   "size" (format "~ax~a"
                                                                                  width height))]
                                                          [r (if (= t-length +inf.0)
                                                                 (hash-set r "d" 9999999)
                                                                 (hash-set r "d" (exact->inexact
                                                                                  t-length)))])
                                                     r))
                                 'audio (mk-empty-audio-filter #:duration len-a))
                           #:counts target-counts))
                        (define pad-b
                          (mk-filter-node
                           (hash 'video (mk-filter "fade"
                                                   (hash "t" "in"
                                                         "st" 0
                                                         "d" fade-length
                                                         "alpha" 1)))
                           #:counts target-counts))
                        (add-vertex! ctx pad-b)
                        (define pts-b
                          (mk-filter-node
                           (hash 'video (mk-filter "setpts"
                                                   (hash "expr" (format "PTS-STARTPTS+(~a/TB)"
                                                                        (- len-a fade-length))))
                                 );'audio (mk-filter "asetpts"
                                 ;                  (hash "expr" (format "PTS-STARTPTS+(~a/TB)"
                                 ;                                       (- len-a fade-length)))))
                           #:counts target-counts))
                        (add-vertex! ctx pts-b)
                        (add-directed-edge! ctx pad-b pts-b 1)
                        (define buff-a1 (mk-fifo-node #:counts target-counts))
                        (define pad-a (mk-fifo-node #:counts target-counts))
                        (define ovr-a
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "amix"
                                                                  (hash "inputs" 2
                                                                        "duration" "shortest")))
                                          #:counts target-counts))
                        (add-vertex! ctx ovr-a)
                        (add-directed-edge! ctx bg-node buff-a1 1)
                        (add-directed-edge! ctx buff-a1 ovr-a 1)
                        (add-directed-edge! ctx pad-a ovr-a 2)
                        (define buff-b1 (mk-fifo-node #:counts target-counts))
                        (define buff-b2 (mk-fifo-node #:counts target-counts))
                        (define ovr-b
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "acrossfade"
                                                                  (hash "d" fade-length)))
                                          #:props (hash "start" 0
                                                        "length" t-length
                                                        "end" t-length
                                                        "width" width
                                                        "height" height)
                                          #:counts target-counts))
                        (add-vertex! ctx ovr-b)
                        (add-directed-edge! ctx ovr-a buff-b1 1)
                        (add-directed-edge! ctx buff-b1 ovr-b 1)
                        (add-directed-edge! ctx pts-b buff-b2 2)
                        (add-directed-edge! ctx buff-b2 ovr-b 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons pad-a pad-b)
                                             #:sinks ovr-b
                                             #:prop (hash "length" t-length))))

(define-merge (overlay-merge x y [w #f] [h #f])
  #:source-props (λ (#:target-props target-props
                     #:target-counts target-counts)
                   (define tw (dict-ref target-props "width" #f))
                   (define th (dict-ref target-props "height" #f))
                   (define s (dict-ref target-props "start" #f))
                   (define e (dict-ref target-props "end" #f))
                   (values (dict-set* target-props
                                      "start" 0
                                      "end" (and s e (- e s))
                                      "pix-fmt" #f
                                      "sample-fmt" #f)
                           target-counts
                           (dict-set* target-props
                                      "start" 0
                                      "end" (and s e (- e s))
                                      "width" tw
                                      "height" th
                                      "pix-fmt" #f
                                      "sample-fmt" #f)
                           target-counts))
  #:combined-subgraph (λ (#:empty-graph ctx
                          #:track1-props t1
                          #:track1-counts c1
                          #:track2-props t2
                          #:track2-counts c2
                          #:target-props target-prop
                          #:target-counts target-counts)
                        (define len (min (- (dict-ref t1 "end" 0)
                                            (dict-ref t1 "start" 0))
                                         (- (dict-ref t2 "end" 0)
                                            (dict-ref t2 "start" 0))))
                        (define buff-node1 (mk-fifo-node #:counts target-counts))
                        (define buff-node2 (mk-fifo-node #:counts target-counts))
                        (define needs-clipping?
                          (or (and (dict-ref t1 "start" #f)
                                   (dict-ref t1 "end" #f))
                              (and (dict-ref t2 "start" #f)
                                   (dict-ref t2 "end" #f))))
                        (define trim-node1
                          (cond [needs-clipping?
                                 (define node (mk-trim-node #:start 0
                                                            #:end len
                                                            #:props t1
                                                            #:counts c1))
                                 (add-vertex! ctx node)
                                 (add-directed-edge! ctx buff-node1 node 1)
                                 node]
                                [else buff-node1]))
                        (define trim-node2
                          (cond [needs-clipping?
                                 (define node (mk-trim-node #:start 0
                                                            #:end len
                                                            #:props t2
                                                            #:counts c2))
                                 (add-vertex! ctx node)
                                 (add-directed-edge! ctx buff-node2 node 1)
                                 node]
                                [else buff-node2]))
                        (define scale-node2
                          (cond
                            [(and w h)
                             (define node
                               (mk-filter-node (hash 'video (mk-filter "scale" (hash "w" w
                                                                                     "h" h)))
                                               #:props t1
                                               #:counts c1))
                             (add-vertex! ctx node)
                             (add-directed-edge! ctx trim-node2 node 1)
                             node]
                            [else trim-node2]))
                        (define overlay
                          (mk-filter-node (hash 'video (mk-filter "overlay" (hash "x" x
                                                                                  "y" y))
                                                'audio (mk-filter "amix"))
                                          #:props (dict-set* t1
                                                             "pix-fmt" #f
                                                             "sample-fmt" #f
                                                             "times-unset?" #t)
                                          #:counts c1))
                        (add-vertex! ctx overlay)
                        (add-directed-edge! ctx trim-node1 overlay 1)
                        (add-directed-edge! ctx scale-node2 overlay 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons buff-node1 buff-node2)
                                             #:sinks overlay)))


(define-merge (composite-merge x1 y1 x2 y2)
  #:combined-subgraph (λ (#:empty-graph ctx
                          #:track1-props t1
                          #:track1-counts c1
                          #:track2-props t2
                          #:track2-counts c2
                          #:target-props target-prop
                          #:target-counts target-counts)
                        (define (exact-min a b)
                          (if (a . < . b) a b))
                        (define (exact-max a b)
                          (if (a . < . b) b a))
                        (define t1s (dict-ref t1 "start" #f))
                        (define t1e (dict-ref t1 "end" #f))
                        (define t2s (dict-ref t2 "start" #f))
                        (define t2e (dict-ref t2 "end" #f))
                        (define start (exact-max (or t1s 0)
                                                 (or t2s 0)))
                        (define end (exact-min (or t1e +inf.0)
                                               (or t2e +inf.0)))
                        (define zero-node1 (mk-reset-timestamp-node
                                            #:props t1
                                            #:counts c1))
                        (define zero-node2 (mk-reset-timestamp-node
                                            #:props t2
                                            #:counts c2))
                        (add-vertex! ctx zero-node1)
                        (add-vertex! ctx zero-node2)
                        ;; Just pick a size
                        (log-video-warning
                         "composite-merge: no resolution on source node, making one up")
                        (define t1w (dict-ref t1 "width" 1920))
                        (define t1h (dict-ref t1 "height" 1080))
                        (define scale-node2
                          (cond
                            [(and t1w t1h)
                             (define scale-node2
                               (mk-filter-node (hash 'video (mk-filter "scale"
                                                                       (hash "w" (* (- x2 x1) t1w)
                                                                             "h" (* (- y2 y1) t1h))))
                                               #:props t2
                                               #:counts c2))
                             (add-vertex! ctx scale-node2)
                             (add-directed-edge! ctx zero-node2 scale-node2 1)
                             scale-node2]
                            [else zero-node2]))
                        (define overlay
                          (mk-filter-node (hash 'video (mk-filter "overlay" (hash "x" (* t1w x1)
                                                                                  "y" (* t1h y1)))
                                                'audio (mk-filter "amix"))
                                          #:counts c1
                                          #:props (dict-set* t1
                                                             "pix-fmt" #f
                                                             "sample-fmt" #f
                                                             "start" start
                                                             "end" end
                                                             "time-unset?" #t)))
                        (add-vertex! ctx overlay)
                        (add-directed-edge! ctx zero-node1 overlay 1)
                        (add-directed-edge! ctx scale-node2 overlay 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons zero-node1 zero-node2)
                                             #:sinks overlay)))

#; ; TODO
(define-transition (swipe-transition #:direction dir)
  #:direction t/b
  (error "TODO"))

(define (scale-filter w h)
  (make-filter #:subgraph (hash 'video (mk-filter "scale" (hash "width" w "height" h)))))

(define (crop-filter x y w h)
  (make-filter #:subgraph (hash 'video (mk-filter "crop" (hash "x" x
                                                               "y" y
                                                               "width" w
                                                               "height" h)))))

(define (pad-filter x y w h)
  (make-filter #:subgraph (hash 'video (mk-filter "pad" (hash "x" x
                                                              "y" y
                                                              "width" w
                                                              "height" h)))))

(define-syntax (external-video stx)
  (syntax-parse stx
    [(_ mod (~optional args))
     (if (attribute args)
         #'(apply
            (dynamic-require
             (module-path-index-join
              mod
              (variable-reference->module-path-index (#%variable-reference)))
             'vidlib)
            args)
         #'(dynamic-require
            (module-path-index-join
             mod
             (variable-reference->module-path-index (#%variable-reference)))
            'vid))]))

(define-syntax (for/playlist stx)
  (syntax-parse stx
    [(_ rest ...)
     #'(apply playlist (for/list rest ...))]))

(define-syntax (for/multitrack stx)
  (syntax-parse stx
    [(_ rest ...)
     #'(apply multitrack (for/list rest ...))]))

(define (cut-producer producer
                      #:start [start #f]
                      #:end [end #f])
  (copy-video producer
              #:prop (let* ([p (properties-prop producer)]
                            [p (if start (dict-set p "start" start) p)]
                            [p (if end (dict-set p "end" end) p)])
                       p)))

(define (transpose-filter [direction #f] [flip #f])
  (make-filter #:subgraph (hash 'video (mk-filter "transpose"
                                                  (hash "dir" (match* (direction flip)
                                                                [('counter-clock #t) "cclock_flip"]
                                                                [('counter-clock #f) "cclock"]
                                                                [('clock #t) "clock_flip"]
                                                                [('clock #f) "clock"]
                                                                [(_ _) "cclock_flip"]))))))

(define (rotate-filter angle
                       #:bilinear? [bilinear? #t]
                       #:fill-color [fillcolor "black"])
  (make-filter #:subgraph (hash 'video (mk-filter rotate
                                                  (hash "a" angle
                                                        "bilinear" (if bilinear? 1 0)
                                                        "c" (color->string fillcolor))))))

(define (color-channel-mixer-filter table)
  (define color-channel-mixer-keys
    (set "rr" "rg" "rb" "ra" "gr" "gg" "gb" "ga" "br" "bg" "bb" "ba" "ar" "ag" "ab" "aa"))
  (for ([(k v) (in-dict table)])
    (unless (set-member? color-channel-mixer-keys k)
      (error 'color-channel-mixer-filter "Invalid key: ~a" k)))
  (make-filter #:subgraph (hash 'video (mk-filter "colorchannelmixer" table))))

(define (grayscale-filter)
  (color-channel-mixer-filter (hash "rr" 0.3 "rg" 0.4 "rb" 0.3 "ra" 0
                                    "gr" 0.3 "gg" 0.4 "gb" 0.3 "ga" 0
                                    "br" 0.4 "bg" 0.4 "bb" 0.3 "ba" 0
                                    "ar" 0   "ag" 0   "ab" 0   "aa" 0)))

(define (sepia-filter)
  (color-channel-mixer-filter (hash "rr" 0.393 "rg" 0.769 "rb" 0.189 "ra" 0
                                    "gr" 0.349 "gg" 0.686 "gb" 0.168 "ga" 0
                                    "br" 0.272 "bg" 0.534 "bb" 0.131 "ba" 0
                                    "ar" 0     "ag" 0     "ab" 0     "aa" 0)))

(define (remove-video)
  (make-filter
   #:subgraph (λ (#:empty-graph ctx
                  #:source-props source-props
                  #:source-counts source-counts)
                (define node
                  (mk-filter-node (hash 'video (mk-filter "nullsink")
                                        'audio (mk-filter "afifo"))
                                  #:props source-props
                                  #:counts (hash-set source-counts 'video 0)))
                (add-vertex! ctx node)
                (make-video-subgraph
                 #:graph ctx
                 #:sources node
                 #:sinks node
                 #:prop source-props))))

(define (remove-audio)
  (make-filter
   #:subgraph (λ (#:empty-graph ctx
                  #:source-props source-props
                  #:source-counts source-counts)
                (define node
                  (mk-filter-node (hash 'video (mk-filter "fifo")
                                        'audio (mk-filter "anullsink"))
                                  #:props source-props
                                  #:counts (hash-set source-counts 'audio 0)))
                (add-vertex! ctx node)
                (make-video-subgraph
                 #:graph ctx
                 #:sources node
                 #:sinks node
                 #:prop source-props))))

(define ((pass-filter-helper type)
         #:frequency [f #f]
         #:poles [p #f]
         #:width [w #f]
         #:width-type [t 'h]
         #:channels [c #f])
  (make-filter #:subgraph (hash 'audio
                                (mk-filter type
                                           (let* ([_ (hash)]
                                                  [_ (if f (hash-set _ "f" f) _)]
                                                  [_ (if p (hash-set _ "p" p) _)]
                                                  [_ (if w (hash-set _ "w" w) _)]
                                                  [_ (if w (hash-set _ "t" t) _)])
                                             _)))))

(define lowpass-filter (pass-filter-helper "lowpass"))
(define highpass-filter (pass-filter-helper "highpass"))

(define (compand-filter #:attacks [attacks '()]
                        #:decays [decays '()]
                        #:points [points '()]
                        #:soft-knee [soft-knee #f]
                        #:gain [gain #f]
                        #:volume [volume #f]
                        #:delay [delay #f])
  (define points*
    (for/list ([p (in-list points)])
      (if (pair?  p)
          (format "~a/~a"
                  (runtime:racket->ffmpeg (car p))
                  (runtime:racket->ffmpeg (cdr p)))
          (format "~a/~a"
                  (runtime:racket->ffmpeg (real-part p))
                  (runtime:racket->ffmpeg (imag-part p))))))
  (make-filter #:subgraph (hash 'audio
                                (mk-filter "compand"
                                           (let* ([_ (hash "attacks" attacks
                                                           "decays" decays
                                                           "points" points*)]
                                                  [_ (if soft-knee
                                                         (hash-set _ "soft-knee" soft-knee)
                                                         _)]
                                                  [_ (if gain (hash-set _ "gain" gain) _)]
                                                  [_ (if volume (hash-set _ "volume" volume) _)]
                                                  [_ (if delay (hash-set _ "delay" delay) _)])
                                             _)))))
                         

(define (mux-filter #:type t
                    #:index index)
  (define (mux-proc #:empty-graph ctx
                    #:target-props prev
                    #:target-counts counts)
    (define type (match t
                   [(or 'v 'video) 'video]
                   [(or 'a 'audio) 'audio]))
    (define mux
      (mk-mux-node type index counts
                   #:props prev
                   #:counts (hash type 1)))
    (add-vertex! ctx mux)
    (make-video-subgraph #:graph ctx
                         #:sources mux
                         #:sinks mux
                         #:prop prev))
  (make-filter #:subgraph mux-proc))

(define (envelope-filter #:direction direction
                         #:length length
                         #:curve [curve #f])
  (define (envelope-proc #:empty-graph ctx
                         #:target-props prev
                         #:target-counts c)
    (define start (dict-ref prev "start" 0))
    (define end (dict-ref prev "end" 0))
    (define table
      (let* ([v (match direction
                  ['in (hash "t" "in"
                             "ss" 0
                             "d" length)]
                  ['out (hash "t" "out"
                              "ss" (- end length)
                              "d" length)])]
             [v (if curve
                    (hash-set v "curve" (symbol->string curve))
                    v)])
        v))
    (define fade-filter
      (mk-filter "afade" table))
    (define node
      (mk-filter-node (hash 'audio fade-filter)
                      #:counts c
                      #:props prev))
    (add-vertex! ctx node)
    (make-video-subgraph #:graph ctx
                         #:sources node
                         #:sinks node
                         #:prop prev))
  (make-filter #:subgraph envelope-proc))

(define (chapter prod)
  (set-property
   prod "chapters"
   (cons (make-chapter #:start (get-property prod "start")
                       #:end (get-property prod "end"))
         (get-property prod "chapters" '()))))

;; ===================================================================================================
;; Helpers used by this module (not provided)
;; ===================================================================================================

;; Convert a path relative to a video file to
;; a string that is the absolute path.
;; Path -> String
(define (relative-path->string path)
  (path->string (path->complete-path path (current-video-directory))))

;; Dispatch on the type of value given to it
;; any/c -> Nonnegative-Integer
(define (unit-dispatch prod prop val)
  val)
#|
  (cond
    [(pixel? val)
     (* (pixel-val val) (get-property prod prop))]
    [else
     (inexact->exact (round (* val 100)))]))
|#
