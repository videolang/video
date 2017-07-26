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
         racket/class
         racket/draw
         racket/match
         racket/format
         racket/list
         racket/set
         racket/math
         racket/dict
         syntax/location
         (except-in pict frame blank)
         graph
         "private/video.rkt"
         "surface.rkt"
         (prefix-in core: "private/video.rkt")
         "units.rkt"
         (for-syntax syntax/parse
                     racket/base
                     racket/syntax))

(provide
 (contract-out
  ;; Creates a multitrack (tracks playing in parallel
  ;;   (not quite sure what right interface for this function
  ;;   looks like yet)
  [multitrack (->* []
                   [#:transitions (listof field-element?)
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
  [blank (-> (or/c nonnegative-integer? #f) (or/c blank? producer?))]
  
  ;; Creates a producer that plays a clip from a file
  [clip (->producer [(or/c path-string? path?)]
                    [])]

  ;; Creates a producer that is a solid color
  [color (->producer [(or/c string? (is-a?/c color%)
                            (list/c byte? byte? byte?))]
                     [])]

  ;; Create a producer that is the same as the other producer but with one or more
  ;; filters attached to it
  [attach-filter (-> service? filter? ... producer?)]

  ;; Creates a clip who's producer is path
  [image (->producer [(or/c path-string? path-for-some-system?)]
                     [])]

  ;; Creates a fading transition from a start to end clip
  [fade-transition (->transition []
                                 [(and/c number? positive?)]
                                 #:direction s/e)]

  [overlay-transition (->transition [(and/c number? positive?)
                                     (and/c number? positive?)]
                                    [(or/c (and/c number? positive?) #f)
                                     (or/c (and/c number? positive?) #f)]
                                    #:direction t/b)]
  
  ;; Creates a composite transition where the top track is
  ;;   placed above the bottom track
  [composite-transition (->transition [(between/c 0 1)
                                       (between/c 0 1)
                                       (between/c 0 1)
                                       (between/c 0 1)]
                                       []
                                       #:direction t/b)]

  #| TODO
  ;; Creates a swiping transition from a start clip to an
  ;;   end clip
  [swipe-transition (->transition [#:direction symbol?]
                                  []
                                  #:direction t/b)]
|#
  
  [scale-filter (-> (and/c number? positive?) (and/c number? positive?) filter?)]

  [color-channel-mixer-filter (-> (hash/c string? (between/c -2 2)) filter?)]

  [grayscale-filter (-> filter?)]

  [sepia-filter (-> filter?)]

  [mux-filter (-> #:type (or/c 'video 'v 'audio 'a)
                  #:index nonnegative-integer?
                  filter?)]

  ;; Set a property associated with a properties struct
  [set-property (-> properties? string? any/c properties?)]

  ;; Get a property associated with a properties struct
  [get-property (->* [properties? string?]
                     [symbol?]
                     any/c)]

  ;; Generate a new video with a new in-out
  [cut-producer (->* [producer?]
                     [#:start (or/c nonnegative-integer? #f)
                      #:end (or/c nonnegative-integer? #f)]
                     producer?)]

  ;; Envelope filter for audio tracks
  [envelope-filter (-> #:length nonnegative-integer?
                       #:direction (or/c 'in 'out)
                       filter?)])

  external-video)

(define (blank length)
  (if length
      (make-blank #:prop (hash "start" 0
                               "end" length))
      (color "black")))

(define (clip path
              #:filters [filters #f]
              #:properties [properties #f])
  (define clip-path (relative-path->string path))
  (make-file #:path clip-path
             #:prop (or properties (hash))
             #:filters (or filters '())))

(define (image path
               #:filters [filters #f]
               #:properties [properties #f])
  (clip path #:filters filters #:properties properties))

(define-producer (color c)
  #:user-properties prop
  #:subgraph (hash 'video
                   (mk-filter "color" (let* ([ret (hash "c" (color->string c))]
                                             [ret (if length (hash-set ret "d" length) ret)]
                                             [ret (if (and width height)
                                                      (hash-set ret "size" (format "~ax~a"
                                                                                   width
                                                                                   height))
                                                      ret)])
                                        ret))
                   'audio
                   (mk-empty-audio-filter #:duration length))
  (define length (or (dict-ref prop "length" #f)
                     (and (dict-ref prop "start" #f)
                          (dict-ref prop "end" #f)
                          (- (dict-ref prop "end") (dict-ref prop "start")))))
  (define width (dict-ref prop "width" #f))
  (define height (dict-ref prop "height" #f)))

(define (multitrack #:transitions [transitions '()]
                    #:properties [prop #f]
                    #:filters [maybe-filters #f]
                    . tracks)
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
  (make-multitrack #:tracks (reverse tracks*)
                   #:field transitions*
                   #:prop (or prop (hash))
                   #:filters (or maybe-filters '())))

(define (playlist #:transitions [transitions '()]
                  #:properties [prop #f]
                  #:filters [maybe-filters #f]
                  . clips)
  (make-playlist
   #:elements
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
   #:prop (or prop (hash))
   #:filters (or maybe-filters '())))

(define (attach-filter obj . f)
  (define new-filters (append f (service-filters obj)))
  (copy-video obj #:filters new-filters))

(define (set-property obj key val)
  (define new-props (hash-set (properties-prop obj) key val))
  (copy-video obj #:prop new-props))

(define-transition (fade-transition [fade-length 1])
  #:direction s/e
  #:properties (λ (prop)
                 (dict-set* (or prop (hash))
                            "pre-length" fade-length
                            "post-length" fade-length))
  #:track1-subgraph (λ (ctx node-a)
                      #f)
  #:track2-subgraph (λ (ctx node-b)
                      #f)
  #:combined-subgraph (λ (ctx node-a node-b)
                        (define width (max (get-property node-a "width" 0)
                                           (get-property node-b "width" 0)))
                        (define height (max (get-property node-a "height" 0)
                                            (get-property node-b "height" 0)))
                        (define len-a (- (get-property node-a "end")
                                         (get-property node-a "start")))
                        (define len-b (- (get-property node-b "end")
                                         (get-property node-b "start")))
                        (define t-length (- (+ len-a len-b) fade-length))
                        (define bg-node
                          (mk-filter-node
                           (hash 'video (mk-filter "color" (hash "color" "black"
                                                                 "size" (format "~ax~a" width height)
                                                                 "d" (exact->inexact t-length)))
                                 'audio (mk-empty-audio-filter #:duration len-a))
                           #:counts (node-counts node-a)))
                        (define pad-a
                          (mk-filter-node (hash 'video (mk-filter "scale" (hash "width" width
                                                                              "height" height)))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pad-a)
                        (define pad-b
                          (mk-filter-node (hash 'video (mk-filter "scale" (hash "width" width
                                                                              "height" height)))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pad-b)
                        (define pts-a
                          (mk-filter-node (hash 'video (mk-filter "setpts"
                                                                  (hash "expr" "PTS-STARTPTS"))
                                                'audio (mk-filter "asetpts"
                                                                  (hash "expr" "PTS-STARTPTS")))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx pts-a)
                        (add-directed-edge! ctx pad-a pts-a 1)
                        (define pre-pts-b
                          (mk-filter-node (hash 'video (mk-filter "setpts"
                                                                  (hash "expr" "PTS-STARTPTS"))
                                                'audio (mk-filter "asetpts"
                                                                  (hash "expr" "PTS-STARTPTS")))
                                          #:counts (node-counts node-b)))
                        (add-vertex! ctx pre-pts-b)
                        (add-directed-edge! ctx pad-b pre-pts-b 1)
                        (define fade-b
                          (mk-filter-node
                           (hash 'video (mk-filter "fade"
                                                   (hash "t" "in"
                                                         "st" 0
                                                         "d" fade-length
                                                         "alpha" 1)))
                           #:counts (node-counts node-b)))
                        (add-vertex! ctx fade-b)
                        (add-directed-edge! ctx pre-pts-b fade-b 1)
                        (define pts-b
                          (mk-filter-node
                           (hash 'video (mk-filter "setpts"
                                                   (hash "expr" (format "PTS-STARTPTS+(~a/TB)"
                                                                        (- len-a fade-length)))))
                           #:counts (node-counts node-b)))
                        (add-vertex! ctx pts-b)
                        (add-directed-edge! ctx fade-b pts-b 1)
                        (define buff-a1 (mk-fifo-node #:counts (node-counts node-a)))
                        (define buff-a2 (mk-fifo-node #:counts (node-counts node-a)))
                        (define ovr-a
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "amix"
                                                                  (hash "inputs" 2
                                                                        "duration" "shortest")))
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx ovr-a)
                        (add-directed-edge! ctx bg-node buff-a1 1)
                        (add-directed-edge! ctx buff-a1 ovr-a 1)
                        (add-directed-edge! ctx pts-a buff-a2 2)
                        (add-directed-edge! ctx buff-a2 ovr-a 2)
                        (define buff-b1 (mk-fifo-node #:counts (node-counts node-a)))
                        (define buff-b2 (mk-fifo-node #:counts (node-counts node-a)))
                        (define ovr-b
                          (mk-filter-node (hash 'video (mk-filter "overlay")
                                                'audio (mk-filter "acrossfade"
                                                                  (hash "d" fade-length)))
                                          #:props (hash "start" 0
                                                        "length" t-length
                                                        "end" t-length
                                                        "width" width
                                                        "height" height)
                                          #:counts (node-counts node-a)))
                        (add-vertex! ctx ovr-b)
                        (add-directed-edge! ctx ovr-a buff-b1 1)
                        (add-directed-edge! ctx buff-b1 ovr-b 1)
                        (add-directed-edge! ctx pts-b buff-b2 2)
                        (add-directed-edge! ctx buff-b2 ovr-b 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons pad-a pad-b)
                                             #:sinks ovr-b
                                             #:prop (hash "length" t-length))))

(define-transition (overlay-transition x y [w #f] [h #f])
  #:direction t/b
  #:track1-subgraph (λ (ctx t1) #f)
  #:track2-subgraph (λ (ctx t2) #f)
  #:combined-subgraph (λ (ctx t1 t2)
                        (define zero-node1 (mk-reset-timestamp-node
                                            #:props (node-props t1)
                                            #:counts (node-counts t1)))
                        (define zero-node2 (mk-reset-timestamp-node
                                            #:props (node-props t2)
                                            #:counts (node-counts t2)))
                        (add-vertex! ctx zero-node1)
                        (add-vertex! ctx zero-node2)
                        (define length (min (- (get-property t1 "end" 0)
                                               (get-property t1 "start" 0))
                                            (- (get-property t2 "end" 0)
                                               (get-property t2 "start" 0))))
                        (define needs-clipping?
                          (or (and (get-property t1 "start" #f)
                                   (get-property t1 "end" #f))
                              (and (get-property t2 "start" #f)
                                   (get-property t2 "end" #f))))
                        (define trim-node1
                          (cond [needs-clipping?
                                 (define node (mk-trim-node #:start 0
                                                            #:end length
                                                            #:props (node-props t1)
                                                            #:counts (node-counts t1)))
                                 (add-vertex! ctx node)
                                 (add-directed-edge! ctx zero-node1 node 1)
                                 node]
                                [else zero-node1]))
                        (define trim-node2
                          (cond [needs-clipping?
                                 (define node (mk-trim-node #:start 0
                                                            #:end length
                                                            #:props (node-props t2)
                                                            #:counts (node-counts t2)))
                                 (add-vertex! ctx node)
                                 (add-directed-edge! ctx zero-node2 node 2)
                                 node]
                                [else zero-node2]))
                        (define scale-node2
                          (cond
                            [(and w h)
                             (define node
                               (mk-filter-node (hash 'video (mk-filter "scale" (hash "w" w
                                                                                     "h" h)))
                                               #:props (node-props t1)
                                               #:counts (node-counts t1)))
                             (add-vertex! ctx node)
                             (add-directed-edge! ctx trim-node2 node 1)
                             node]
                            [else trim-node2]))
                        (define overlay
                          (mk-filter-node (hash 'video (mk-filter "overlay" (hash "x" x
                                                                                  "y" y))
                                                'audio (mk-filter "amix"))
                                          #:counts (node-counts t1)
                                          #:props (node-props t1)))
                        (add-vertex! ctx overlay)
                        (add-directed-edge! ctx trim-node1 overlay 1)
                        (add-directed-edge! ctx scale-node2 overlay 2)
                        (make-video-subgraph #:graph ctx
                                             #:sources (cons zero-node1 zero-node2)
                                             #:sinks overlay)))


(define-transition (composite-transition x1 y1 x2 y2)
  #:direction t/b
  #:track1-subgraph (λ (ctx t1) #f)
  #:track2-subgraph (λ (ctx t2) #f)
  #:combined-subgraph (λ (ctx t1 t2)
                        (define zero-node1 (mk-reset-timestamp-node
                                            #:props (node-props t1)
                                            #:counts (node-counts t1)))
                        (define zero-node2 (mk-reset-timestamp-node
                                            #:props (node-props t2)
                                            #:counts (node-counts t2)))
                        (add-vertex! ctx zero-node1)
                        (add-vertex! ctx zero-node2)
                        (define t1w (get-property t1 "width"))
                        (define t1h (get-property t1 "height"))
                        (define scale-node2
                          (mk-filter-node (hash 'video (mk-filter "scale"
                                                                  (hash "w" (* (- x2 x1) t1w)
                                                                        "h" (* (- y2 y1) t1h))))
                                          #:props (node-props t2)
                                          #:counts (node-counts t2)))
                        (add-vertex! ctx scale-node2)
                        (add-directed-edge! ctx zero-node2 scale-node2 1)
                        (define overlay
                          (mk-filter-node (hash 'video (mk-filter "overlay" (hash "x" (* t1w x1)
                                                                                  "y" (* t1h y1)))
                                                'audio (mk-filter "amix"))
                                          #:counts (node-counts t1)
                                          #:props (node-props t1)))
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
  (error "TODO"))

(define-syntax (external-video stx)
  (syntax-parse stx
    [(_ mod
        (~or (~optional (~seq #:start start*) #:defaults ([start* #'#f]))
             (~optional (~seq #:end end*) #:defaults ([end* #'#f]))
             (~optional (~seq #:length length) #:defaults ([length #'#f])))
        ...)
     #'(let ()
         (define start (or start* (and length 0)))
         (define end (or end* length))
         (let* ([vid (dynamic-require
                      (module-path-index-join
                       mod
                       (variable-reference->module-path-index (#%variable-reference)))
                      'vid)]
                [vid (if start (set-property vid "start" start) vid)]
                [vid (if end (set-property vid "end" end) vid)])
           vid))]))

(define (cut-producer producer
                      #:start [start #f]
                      #:end [end #f])
  (copy-video producer
              #:prop (let* ([p (properties-prop producer)]
                            [p (if start (dict-set p "start" start) p)]
                            [p (if end (dict-set p "end" end) p)])
                       p)))

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

(define (mux-filter #:type t
                    #:index index)
  (define (mux-proc ctx prev)
    (define type (match t
                   [(or 'v 'video) 'video]
                   [(or 'a 'audio) 'audio]))
    (define mux
      (mk-mux-node type index (node-counts prev)
                   #:props (node-props prev)
                   #:counts (hash type 1)))
    (add-vertex! ctx mux)
    (make-video-subgraph #:graph ctx
                         #:sources mux
                         #:sinks mux
                         #:prop (node-props prev)))
  (make-filter #:subgraph mux-proc))

(define (envelope-filter #:direction direction
                         #:length length)
  (define (envelope-proc ctx prev)
    (define start (get-property prev "start" 0))
    (define end (get-property prev "end" 0))
    (define fade-filter
      (mk-filter "fade" (match direction
                          ['in (hash "t" "in"
                                     "ss" 0
                                     "d" length)]
                          ['out (hash "t" "out"
                                      "ss" (- end length)
                                      "d" length)])))
    (define node
      (mk-filter-node (hash 'audio fade-filter)
                      #:counts (node-counts prev)
                      #:props (node-props prev)))
    (add-vertex! ctx node)
    (make-video-subgraph #:graph ctx
                         #:sources node
                         #:sinks node
                         #:prop (node-props prev)))
  (make-filter #:subgraph envelope-proc))

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
