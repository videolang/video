#lang racket/base

#|
   Copyright 2016-2018 Leif Andersen

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
         racket/port
         racket/hash
         racket/list
         graph
         (except-in ffi/unsafe ->)
         (prefix-in base: racket/base)
         (prefix-in file: file/convertible)
         (only-in pict pict? pict->bitmap)
         racket/async-channel
         "private/init.rkt"
         "private/ffmpeg/main.rkt"
         "private/ffmpeg-pipeline.rkt"
         (prefix-in ffmpeg: "private/ffmpeg-pipeline.rkt")
         (prefix-in video: "private/video.rkt")
         "private/utils.rkt"
         "private/log.rkt"
         "private/render-settings.rkt"
         "convert.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide
 (contract-out
  ;; A single threaded interface to the render% object.
  ;; Simplest way to render a video, just give it an input and destination.
  ;; This function is not very extendable. For example, it is impossible to get
  ;; rendering progress using it.
  [render (->* [any/c]
               [(or/c path-string? path? #f)
                #:render-mixin (or/c (-> render<%>/c render<%>/c) #f)
                #:convert-database (or/c (is-a?/c convert-database%) #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps real?
                #:format (or/c symbol? #f)
                #:video-frames (or/c nonnegative-integer? #f)
                #:audio-frames (or/c nonnegative-integer? #f)
                #:data-frames (or/c nonnegative-integer? #f)
                #:pix-fmt symbol?
                #:sample-fmt symbol?
                #:render-video? boolean?
                #:render-audio? boolean?
                #:start (or/c (and/c real? (>=/c 0)) #f)
                #:end (or/c (and/c real? (>=/c 0)) #f)
                #:probe? boolean?]
               void?)]
 
  ;; Similar to render, however non-blocking.
  ;; Instead, this function immediately returns a channel which will
  ;;   frequently reports the current rebderubg position. Once the renderer
  ;;   has finished, it will put eof signalling that no new input will be
  ;;   placed on the channel.
  [render/async (->* [any/c]
                     [(or/c path-string? path? #f)
                      #:render-mixin (or/c (-> render<%>/c render<%>/c) #f)
                      #:convert-database (or/c (is-a?/c convert-database%) #f)
                      #:width (and/c integer? positive?)
                      #:height (and/c integer? positive?)
                      #:fps real?
                      #:format (or/c symbol? #f)
                      #:start (or/c (and/c real? (>=/c 0)) #f)
                      #:end (or/c (and/c real? (>=/c 0)) #f)
                      #:video-frames (or/c nonnegative-integer? #f)
                      #:audio-frames (or/c nonnegative-integer? #f)
                      #:data-frames (or/c nonnegative-integer? #f)
                      #:pix-fmt symbol?
                      #:sample-fmt symbol?
                      #:render-video? boolean?
                      #:render-audio? boolean?
                      #:mode (or/c 'verbose #f)
                      #:probe? boolean?]
                     (values async-channel? (-> void?)))]

  ;; A hybrid of render and render/async. This version blocks until the
  ;;   the video is finished rendering. However, it optionally takes an output
  ;;   port that it writes to. This is useful for having an automatic progress bar
  ;;   sent to the console.
  [render/pretty (->* [any/c]
                      [(or/c path-string? path? #f)
                       #:render-mixin (or/c (-> render<%>/c render<%>/c) #f)
                       #:convert-database (or/c (is-a?/c convert-database%) #f)
                       #:width (and/c integer? positive?)
                       #:height (and/c integer? positive?)
                       #:fps real?
                       #:format (or/c symbol? #f)
                       #:start (or/c (and/c real? (>=/c 0)) #f)
                       #:end (or/c (and/c real? (>=/c 0)) #f)
                       #:port (or/c output-port? #f)
                       #:video-frames (or/c nonnegative-integer? #f)
                       #:audio-frames (or/c nonnegative-integer? #f)
                       #:data-frames (or/c nonnegative-integer? #f)
                       #:pix-fmt symbol?
                       #:sample-fmt symbol?
                       #:render-video? boolean?
                       #:render-audio? boolean?
                       #:mode (or/c 'verbose 'silent #f)
                       #:probe? boolean?]
                      void?)]

 ;; The render% object is _NOT_ threadsafe. However, it is important ot use threads when calling
 ;;   certain methods. In particular, `feed-buffers` and `write-results` should be called
 ;;   in seperate threads simultaniously.
 ;; However, no method can be called in more than one thread at a time. (This class should
 ;;    be a monitor.)
 ;; A render object stores the source video as state, but the output is set with the `setup` method.
 ;; Only one setup profile can be used at a time. If multiple are needed, copy the renderer with the
 ;;   copy method.
 ;;
 ;;
 ;; The use case for a render% is along the lines of:
 ;;
 ;; (define r (new render% [source <source>))
 ;; (send r setup #:dest "out.mp4" <other args> ...)
 ;; (thread (λ () (send r feed-buffers)))
 ;; (thread (λ () (send r write-output)))
 ;; (let loop ()
 ;;    (when (send r rendering?)
 ;;      (displayln (send r get-current-position?))
 ;;      (loop)))
 ;; 
 [render% render<%>/c]

 [render-settings? (-> any/c void?)]

 [make-render-settings (->* ()
                            (#:destination (or/c path? path-string? #f)
                             #:split-av boolean?
                             #:width (and/c integer? positive?)
                             #:height (and/c integer? positive?)
                             #:start (or/c (and/c real? (>=/c 0)) #f)
                             #:end (or/c (and/c real? (>=/c 0)) #f)
                             #:fps real?
                             #:format (or/c symbol? #f)
                             #:video-codec (or/c symbol? #f)
                             #:audio-codec (or/c symbol? #f)
                             #:subtitle-codec (or/c symbol? #f)
                             #:pix-fmt symbol?
                             #:sample-fmt symbol?
                             #:sample-rate (and/c real? positive?)
                             #:channel-layout symbol?
                             #:speed real?
                             #:video-frames (or/c nonnegative-integer? #f)
                             #:audio-frames (or/c nonnegative-integer? #f)
                             #:data-frames (or/c nonnegative-integer? #f)
                             #:render-video? boolean?
                             #:render-audio? boolean?
                             #:seek-point (or/c nonnegative-integer? #f))
                            render-settings?)])

 ;; Interface for render%
 render<%>
 render<%>/c)

(define (render video
                [dest #f]
                #:render-mixin [render-mixin #f]
                #:convert-database [convert-database #f]
                #:width [width 1920]
                #:height [height 1080]
                #:format [format #f]
                #:start [start #f]
                #:end [end #f]
                #:pix-fmt [pf 'yuv420p]
                #:sample-fmt [sf 'fltp]
                #:video-frames [vf #f]
                #:audio-frames [af #f]
                #:data-frames [df #f]
                #:render-video? [rv? #t]
                #:render-audio? [ra? #t]
                #:fps [fps 25]
                #:probe? [probe? #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define r% ((or render-mixin values) render%))
  (define r
    (new r%
         [source video]
         [convert-database convert-database]))
  (send r setup
        (make-render-settings #:destination dest*
                              #:width width
                              #:height height
                              #:format format
                              #:start start
                              #:end end
                              #:video-frames vf
                              #:audio-frames af
                              #:data-frames df
                              #:pix-fmt pf
                              #:sample-fmt sf
                              #:render-video? rv?
                              #:render-audio? ra?
                              #:fps fps))
  (if probe?
      (send r dump-info)
      (send r start-rendering #t)))

(define (render/async video
                      [dest #f]
                      #:render-mixin [render-mixin #f]
                      #:convert-database [convert-database #f]
                      #:width [width 1920]
                      #:height [height 1080]
                      #:format [format #f]
                      #:start [start #f]
                      #:end [end #f]
                      #:fps [fps 25]
                      #:video-frames [vf #f]
                      #:audio-frames [af #f]
                      #:data-frames [df #f]
                      #:pix-fmt [pf 'yuv420p]
                      #:sample-fmt [sf 'fltp]
                      #:render-video? [rv? #t]
                      #:render-audio? [ra? #t]
                      #:mode [mode #f]
                      #:probe? [probe? #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define channel (make-async-channel))
  (define r% ((or render-mixin values) render%))
  (define r
    (new r%
         [source video]
         [convert-database convert-database]))
  (send r setup
        (make-render-settings #:destination dest*
                              #:width width
                              #:height height
                              #:start start
                              #:end end
                              #:format format
                              #:video-frames vf
                              #:audio-frames af
                              #:data-frames df
                              #:pix-fmt pf
                              #:sample-fmt sf
                              #:render-video? rv?
                              #:render-audio? ra?
                              #:fps fps))
  (cond [probe?
         (send r dump-info)
         (async-channel-put channel eof)
         (values channel void)]
        [else
         (when (eq? mode 'verbose)
           (async-channel-put channel (send r get-render-graph)))
         (thread
          (λ ()
            (send r start-rendering)
            (let loop ()
              (when (send r rendering?)
                (async-channel-put channel (send r get-current-position))
                (sleep 0.01)
                (loop)))
            (async-channel-put channel eof)))
         (values channel
                 (λ ()
                   (send r stop-rendering)
                   (void)))]))

(define (render/pretty video
                       [dest #f]
                       #:render-mixin [render-mixin #f]
                       #:convert-database [convert-database #f]
                       #:width [width 1920]
                       #:height [height 1080]
                       #:format [format #f]
                       #:start [start #f]
                       #:end [end #f]
                       #:fps [fps 25]
                       #:port [port* (current-output-port)]
                       #:video-frames [vf #f]
                       #:audio-frames [af #f]
                       #:data-frames [df #f]
                       #:pix-fmt [pf 'yuv420p]
                       #:sample-fmt [sf 'fltp]
                       #:render-video? [rv? #t]
                       #:render-audio? [ra? #t]
                       #:mode [mode #f]
                       #:probe? [probe? #f])
  (define port (or (and (not (eq? mode 'silent)) port*)
                   (open-output-nowhere)))
  (define-values (channel stop)
    (render/async video dest
                  #:render-mixin render-mixin
                  #:convert-database convert-database
                  #:width width
                  #:height height
                  #:format format
                  #:start start
                  #:end end
                  #:fps fps
                  #:video-frames vf
                  #:audio-frames af
                  #:data-frames df
                  #:pix-fmt pf
                  #:sample-fmt sf
                  #:render-video? rv?
                  #:render-audio? ra?
                  #:mode (if (eq? mode 'silent) #f mode)
                  #:probe? probe?))
  (when (eq? mode 'verbose)
    (displayln (graphviz (async-channel-get channel))))
  (with-handlers ([exn:break?
                   (λ (e)
                     (newline)
                     (displayln "Cleaning up...")
                     (stop)
                     (displayln "Finished"))])
    (let loop ()
      (define pos (async-channel-get channel))
      (unless (eof-object? pos)
        (fprintf port "\r~a" (real->decimal-string pos))
        (loop))))
  (newline port))

(define render%
  (class object%
    (super-new)
    (init-field source
                [convert-database #f])

    (field [video-graph #f]
           [video-sink #f]
           [render-graph #f]
           [output-node #f]
           [video-start 0]
           [video-end #f]
           [video-seek-point #f])

    (define render-audio? #t)
    (define render-video? #t)
    (define manual-rendering-enable? #f)
    
    (define graph-obj #f)
    (define input-bundles #f)
    (define output-bundles #f)

    (define current-render-status (mk-render-status-box))

    ;; Flag states:
    ;; #f - The reader/writer can continue running
    ;; 'pause - The reader/writer can continue, but should sleep until the flag returns to #t.
    ;; #t - The reader/writer should make a best effort attempt to close down
    (field [reading-thread #f]
           [writing-thread #f]
           [stop-rendering-flag #t])

    (define current-render-settings-lock (make-semaphore 1))
    (define current-render-settings (make-render-settings))
    (define current-mirror-render-settings '())


    ;; Copy this renderer, that way it can be
    ;; setup to multiple profiles
    (define/public (copy)
      (new render% [source source]))

    ;; Generates many of the properties the bundle
    ;;   will need to generate a sink node.
    ;; (Primarily only used once, but used repeatedly
    ;;   in case of mirror settings.)
    (define/private (bundle-props settings
                                  #:render-tag [rt #f])
      (match-define (struct* render-settings ([destination dest]
                                              [split-av split-av]
                                              [width width]
                                              [height height]
                                              [start start*]
                                              [end end*]
                                              [fps fps]
                                              [video-time-base video-time-base]
                                              [audio-time-base audio-time-base]
                                              [format format]
                                              [video-codec video-codec]
                                              [audio-codec audio-codec]
                                              [render-video? render-video?*]
                                              [render-audio? render-audio?*]
                                              [pix-fmt pix-fmt]
                                              [sample-fmt sample-fmt]
                                              [sample-rate sample-rate]
                                              [channel-layout channel-layout]
                                              [speed speed]
                                              [seek-point seek-point]))
        settings)
      ;; Raw videos should output two streams, one for video and one for audio.
      ;; Needed because there is no single container for raw video
      ;;   and audio.
      (define extensions
        (match* (format split-av)
          [('raw _) (append (if render-video? '("video.raw") '())
                            (if render-audio? '("audio.raw") '()))]
          [(_ #t) (append (if render-video? '("video") '())
                          (if render-audio? '("audio") '()))]
          [(_ _) (list "mp4")]))
      ;; If the output type is "raw", generate two feeds, one for audio and one for video
      (define bundle-specs
        (cond
          [(or (eq? format 'raw)
               split-av)
           (append (if render-video? (list 'video) '())
                   (if render-audio? (list 'audio) '()))]
          [else (cond [(and render-video? render-audio?) (list 'vid+aud)]
                      [render-video? (list 'video)]
                      [render-audio? (list 'audio)]
                      [else '()])]))
      ;; When outputting multiple streams,
      ;;   every node but the last one must tell
      ;;   what streams they are consuming.
      ;; Because this list happens in reverse order,
      ;;   only the first element of the list can be #f, for
      ;;   'consume everything else'
      (define consume-tables
        (cond [(or (eq? format 'raw)
                   split-av)
               (if (or render-video? render-audio?)
                   (list #f (hash 'audio 1))
                   (list #f))]
              [else (list #f)]))
      (define format-names
        (cond
          [(eq? format 'raw)
           (append (if render-video? '("rawvideo") '())
                   (if render-audio? '("s16be") '()))]
          [split-av (append (if render-video?
                                (list (and format (symbol->string format)))
                                '())
                            (if render-audio?
                                (list (and format (symbol->string format)))
                                '()))]
          [else (list (and format (symbol->string format)))]))
      (define video-streams (if render-video? 1 0))
      (define audio-streams (if render-audio? 1 0))
      (define target-counts (hash 'video video-streams
                                  'audio audio-streams))
      (define out-paths
        (for/list ([extension (in-list extensions)])
          (define file-str
            (cond
              [split-av
               (define name (or dest "out"))
               (define end (or (and format (symbol->string format)) "mp4"))
               (base:format "~a.~a.~a" name extension end)]
              [else (or dest (base:format "out.~a" extension))]))
          (path->complete-path file-str)))
      (define target-props
        (hash-set* (hash)
                   "width" width
                   "height" height
                   "display-aspect-ratio" (and width height
                                               (/ width height))
                   "start" start*
                   "end" end*
                   "fps" (or fps 25)
                   "video-time-base" (or video-time-base (or (and fps (/ 1 fps)) (/ 1 25)))
                   "audio-time-base" (or audio-time-base (or (and fps (/ 1 fps)) (/ 1 25)))
                   "format" format
                   "pix-fmt" pix-fmt
                   "sample-fmt" sample-fmt
                   "video-codec" video-codec
                   "audio-codec" audio-codec
                   "sample-rate" sample-rate
                   "channel-layout" channel-layout))
      ;; Create out bundle 
      (define out-bundles
        (for/list ([spec (in-list bundle-specs)]
                   [format-name (in-list format-names)]
                   [out-path (in-list out-paths)])
          (stream-bundle->file out-path spec
                               #:format-name format-name
                               #:render-tag rt)))
      (values consume-tables
              target-props
              target-counts
              out-bundles))
    
    ;; Setup the renderer to a specific output context.
    ;; This method can be called multiple times, but it is
    ;; NOT THREADSAFE!
    (define/public (setup settings*)
      (call-with-semaphore
       current-render-settings-lock
       (λ ()
         (define-values (settings mirror-settings)
           (cond [(list? settings*)
                  (values (first settings*)
                          (rest settings*))]
                 [else (values settings* #f)]))
         (match-define (struct* render-settings ([render-video? render-video?*]
                                                 [render-audio? render-audio?*]
                                                 [seek-point seek-point]))
           settings)
         (unless manual-rendering-enable?
           (set! render-video? render-video?*)
           (set! render-audio? render-audio?*))
         (define-values (consume-tables
                         target-props
                         target-counts
                         out-bundles)
           (bundle-props settings #:render-tag 'primary))
         ;; Set properties and start rendering
         (set! video-graph (video:mk-render-graph))
         (set! video-sink (parameterize ([video:current-render-graph video-graph]
                                         [video:current-convert-database convert-database])
                            (video:convert source target-props target-counts)))
         (set! render-graph (graph-copy video-graph))
         (set! current-render-settings settings)
         (define seek-post-proc-node
           (let* ([node video-sink]
                  [node (cond
                          [seek-point
                           (define sn
                             (mk-filter-node
                              (hash 'video (mk-filter "trim"
                                                      (hash "start" (racket->ffmpeg seek-point)))
                                    'audio (mk-filter "atrim"
                                                      (hash "start" (racket->ffmpeg seek-point))))
                              #:props (dict-set* (node-props video-sink)
                                                 "start" seek-point)
                              #:counts (node-counts video-sink)))
                           (add-vertex! render-graph sn)
                           (add-directed-edge! render-graph node sn 1)
                           (define pts-n
                             (mk-filter-node (hash 'video (mk-filter "setpts"
                                                                     (hash "expr" "PTS-STARTPTS"))
                                                   'audio (mk-filter "asetpts"
                                                                     (hash "expr" "PTS-STARTPTS")))
                                             #:props (node-props sn)
                                             #:counts (node-counts sn)))
                           (add-vertex! render-graph pts-n)
                           (add-directed-edge! render-graph sn pts-n 1)
                           pts-n]
                          [else node])])
             node))
         ;; If mirror render settings exist, create a copy node, and link to that.
         (define output-splitter-node
           (cond
             [mirror-settings
              (define split-node
                (mk-split-node #:fan-out (length settings*)
                               #:counts (node-counts video-sink)
                               #:props (node-props video-sink)))
              (add-vertex! render-graph split-node)
              (add-directed-edge! render-graph seek-post-proc-node split-node 1)
              split-node]
             [else seek-post-proc-node]))
         (when mirror-settings
           (for ([mirror (in-list mirror-settings)]
                 [i (in-naturals 2)])
             (define-values (consume-tables
                             target-props
                             target-counts
                             out-bundles)
               (bundle-props mirror #:render-tag `(mirror ,i)))
             (define fifo-node
               (mk-filter-node (hash 'video (mk-fifo-video-filter)
                                     'audio (mk-fifo-audio-filter))
                               #:props (node-props output-splitter-node)
                               #:counts (node-counts output-splitter-node)))
             (add-vertex! render-graph fifo-node)
             (add-directed-edge! render-graph output-splitter-node fifo-node i)
             (define out-node (parameterize ([video:current-render-graph render-graph]
                                             [video:current-convert-database convert-database])
                                (video:convert-filter
                                 (video:make-filter
                                  #:subgraph
                                  (hash 'video (mk-fifo-video-filter)
                                        'audio (mk-fifo-audio-filter)))
                                 target-props
                                 target-counts
                                 #f ; <- not needed? (TODO)
                                 fifo-node)))
             (define mirror-sink
               (for/fold ([next #f])
                         ([out-bundle (in-list out-bundles)]
                          [consume-table (in-list consume-tables)])
                 (mk-sink-node out-bundle
                               #:counts (node-counts video-sink)
                               #:next next
                               #:props (hash-union target-props (node-props out-node)
                                                   #:combine (λ (target user) user))
                               #:consume-table consume-table)))
             (add-vertex! render-graph mirror-sink)
             (add-directed-edge! render-graph out-node mirror-sink 1)))
         (set! current-mirror-render-settings mirror-settings)
         ;; Connect to final node (video sink or copy)
         (define sink-node
           (for/fold ([next #f])
                     ([out-bundle (in-list out-bundles)]
                      [consume-table (in-list consume-tables)])
             (mk-sink-node out-bundle
                           #:counts (node-counts video-sink)
                           #:next next
                           #:props (hash-union target-props (node-props video-sink)
                                               #:combine (λ (target user) user))
                           #:consume-table consume-table)))
         (add-vertex! render-graph sink-node)
         (add-directed-edge! render-graph output-splitter-node sink-node 1)
         (set! output-node sink-node)
         (set! video-start (video:get-property video-sink "start"))
         (set! video-end (video:get-property video-sink "end"))
         (set! video-seek-point seek-point)
         (let-values ([(g i o) (init-filter-graph render-graph)])
           (set! graph-obj g)
           (set! input-bundles i)
           (set! output-bundles o)))))
    
    (define/public (feed-buffers-callback-constructor)
      (filtergraph-insert-packet))

    ;; Send all of the input pads into the render graph.
    ;; This method sould be run in its own thread
    (define/public (feed-buffers)
      (define threads
        (for/list ([bundle (in-list input-bundles)])
          (thread
           (λ ()
             (define callback (feed-buffers-callback-constructor))
             (define demux (new demux%
                                [bundle bundle]
                                [by-index-callback callback]))
             (send demux init)
             ;; Read loop. When paused sleep and pull again.
             (let loop ()
               (sleep 0)
               (case stop-rendering-flag
                 [(#f)
                  (define got-data?
                    (send demux wait-for-packet-need 1))
                  (cond [(not got-data?) (loop)]
                        [(send demux read-packet) (loop)]
                        [else (void)])]
                 [(pause)
                  (sleep 0.1)
                  (loop)]))
             (send demux close)))))
      (for-each thread-wait threads)
      (void))

    ;; Asynchronous thread to start-rendering.
    ;; Simply calls `feed-buffers` and `write-output` (protected
    ;;   methods) in different threads. The `rendering` method
    ;;   determines when those threads stop.
    ;; When wait is true, equivalent to calling `wait-for-rendering`
    ;; after this method concludes.
    ;; Boolean -> Void
    (define/public (start-rendering [wait? #f])
      (define prev-rendering-status
        (or (and reading-thread (not (thread-dead? reading-thread)))
            (and writing-thread (not (thread-dead? reading-thread)))))
      (when prev-rendering-status
        (error 'feed-buffers "Reader already running"))
      (set! stop-rendering-flag #f)
      (set! reading-thread (thread (λ () (feed-buffers))))
      (set! writing-thread (thread (λ () (write-output))))
      (when wait?
        (wait-for-rendering)))

    ;; Wait for rendering to finish, then return.
    (define/public (wait-for-rendering)
      (log-video-debug "Stop rendering: Finishing reading thread")
      (and reading-thread (thread-wait reading-thread))
      (log-video-debug "Stop rendering: Finishing writing thread")
      (and writing-thread (thread-wait writing-thread))
      (log-video-debug "Stop rendering: Complete")
      (void))

    ;; Used for early termination of `feed-buffers`.
    ;; This is needed because trimmed outputs will
    ;;   NEVER finish reading from the inputs.
    ;; Will not return until program is in a stopped state.
    ;;
    ;;
    ;; WARNING: While this method _is_ threadsafe, it is
    ;;   possible to start re-running before thread begins again, depending
    ;;   on your use of the other methods.
    (define/public (stop-rendering [wait #t])
      (set! stop-rendering-flag #t)
      (when wait
        (wait-for-rendering)))

    (define/public (pause-rendering)
      (when (eq? stop-rendering-flag #t)
        (error 'render "Media stopped"))
      (set! stop-rendering-flag 'pause))

    (define/public (resume-rendering)
      (unless (eq? stop-rendering-flag 'pause)
        (error 'render "Media not paused"))
      (set! stop-rendering-flag #f))

    (define/public (paused?)
      (eq? stop-rendering-flag 'pause))

    (define/public (write-output-callback-constructor #:render-status render-status
                                                      #:render-tag render-tag)
      (define-values (video-frames audio-frames data-frames) (values #f #f #f))
      (call-with-semaphore current-render-settings-lock
        (λ ()
          (set! video-frames (render-settings-video-frames current-render-settings))
          (set! audio-frames (render-settings-audio-frames current-render-settings))
          (set! data-frames (render-settings-data-frames current-render-settings))))
      (filtergraph-next-packet #:render-status render-status
                               #:video-frames video-frames
                               #:audio-frames audio-frames
                               #:data-frames data-frames))

    ;; Pull Packets out of the render graph as quickly as possible
    ;; This method sould be run in its own thread
    (define/public (write-output)
      (define threads
        (for/list ([output-bundle (in-list output-bundles)]
                   [index (in-naturals)])
          (thread
           (λ ()
             (define proc (write-output-callback-constructor
                           #:render-status current-render-status
                           #:render-tag (stream-bundle-render-tag output-bundle)))
             (define mux (new mux%
                              [bundle output-bundle]
                              [by-index-callback proc]))
             (send mux init)
             (send mux open)
             (send mux set-chapters
                   (for/list ([chap (in-list (video:get-property video-sink "chapters" '()))]
                              [id (in-naturals)])
                     (define start (inexact->exact (video:chapter-start chap)))
                     (define end (inexact->exact (video:chapter-end chap)))
                     (define base (gcd start end))
                     (define start-num (exact-floor (* base start)))
                     (define end-num (exact-floor (* base end)))
                     (mk-avchapter #:start start-num
                                   #:end end-num
                                   #:time-base (/ 1 base)
                                   #:id id)))
             (send mux write-header)
             ;; Write loop. When paused sleep and pull again.
             (let loop ()
               (sleep 0)
               (case stop-rendering-flag
                 [(#f)
                  (define continue? (send mux write-packet))
                  (when continue?
                    (loop))]
                 [(pause)
                  (sleep 0.1)
                  (loop)]))
             (send mux write-trailer)
             (send mux close)))))
      (map thread-wait threads)
      (set! stop-rendering-flag #t)
      (void))

    (define/public (render-audio val)
      (define r (rendering?))
      (when r
        (stop-rendering))
      (set! manual-rendering-enable? #t)
      (set! render-audio? val)
      (when r
        (setup (struct-copy render-settings current-render-settings
                            [start (get-current-position)]))
        (start-rendering)))

    (define/public (render-video val)
      (define r (rendering?))
      (when r
        (stop-rendering))
      (set! manual-rendering-enable? #t)
      (set! render-video? val)
      (when r
        (setup (struct-copy render-settings current-render-settings
                            [start (get-current-position)]))
        (start-rendering)))

    ;; Seek to a specific position (in seconds). Convience method
    ;;    whose functionality can be duplicated entirely from other methods.
    (define/public (seek position)
      (log-video-debug "Seek requested, position: ~a stopping" position)
      (stop-rendering)
      (log-video-debug "Seek requested, stopped")
      (setup (struct-copy render-settings current-render-settings
                          [seek-point position]))
      (log-video-debug "Seek requested, stopped")
      (start-rendering)
      (log-video-debug "Seek requested, stopped"))

    ;; Convenience method to resize (in pixels) the video. Can be duplicated
    ;;   with external method calls.
    (define/public (resize w h)
      (stop-rendering)
      (setup (struct-copy render-settings current-render-settings
                          [width w]
                          [height h]
                          [start (get-current-position)]))
      (start-rendering))

    ;; Convience method to change speed. 0 is paused, 1 is normal, -1 is normal reversed
    ;;     (0, 1) is slow, (-1, 0) is slow reverse, (1, ∞) is fast
    ;;     (-∞, -1) is fast reverse
    (define/public (set-speed speed)
      (stop-rendering)
      (unless (= speed 0)
        (setup (struct-copy render-settings current-render-settings
                            [speed speed]
                            [start (get-current-position)]))
        (start-rendering)))

    ;; Return a copy of the video-graph. That is, the resulting graph from the given video
    ;;   object. Mutations to this graph will NOT affect the renderer's copy of the graph.
    (define/public (get-video-graph)
      (graph-copy video-graph))

    ;; Returns a copy of the render-graph. This will be the video-graph in addition to
    ;;   the nodes generated by the renderer.
    ;; This result WILL be false until `setup` is called.
    (define/public (get-render-graph)
      (and render-graph (graph-copy render-graph)))

    ;; Determines whether or not the object is rendering.
    ;; Remember that, depending on how you call these methods, it could
    ;;    start rendering after it returns `#f`
    (define/public (rendering?)
      (or (and reading-thread (not (thread-dead? reading-thread)))
          (and writing-thread (not (thread-dead? writing-thread)))))
    
    ;; Possible Results:
    ;; #t   : Rendering
    ;; #f   : Not Rendering
    ;; 'eof : Not Rendering and will not start up again
    (define (internal-status)
      (render-status-box-rendering? current-render-status))

    ;; Get the absolute value of current position of the output stream
    ;;   (presuming that a `write-results` is currently in progress. The result does not change
    ;;   if the stream is trimmed.
    ;; If the video is not being rendered it returns an undefined (numeric or #f) result
    ;; Basically, treat this result as something that will occasionally be wrong
    ;;   (like for a progress bar).
    (define/public (get-current-position)
      (+ (or video-seek-point 0) video-start (render-status-box-position current-render-status)))

    ;; Return the length of the internal video (not the setup length) in seconds
    (define/public (get-length)
      (and video-end video-start
           (- video-end video-start)))

    (define/public (dump-info [file-string #f])
      (for ([bundle (in-list input-bundles)])
        (define callback (feed-buffers-callback-constructor))
        (define demux (new demux%
                           [bundle bundle]
                           [by-index-callback callback]))
        (send demux init)
        (for ([str (in-vector (stream-bundle-streams bundle))]
              [i (in-naturals)])
          (send demux dump-info i))))))

(define render<%>
  (class->interface render%))

;; Prototype contract for render% classes.
(define render<%>/c
  (class/c [copy (->m (instanceof/c (recursive-contract render<%>/c)))]
           [setup (->m (or/c render-settings? (non-empty-listof render-settings?)) void?)]
           [start-rendering (->*m () (boolean?) void?)]
           [wait-for-rendering (->m void?)]
           [stop-rendering (->m void?)]
           [seek (->m (and/c real? (>=/c 0)) void?)]
           [resize (->m (and/c real? positive?) (and/c real? positive?) void?)]
           [render-audio (->m boolean? void?)]
           [render-video (->m boolean? void?)]
           [set-speed (->m real? void?)]
           [get-video-graph (->m graph?)]
           [get-render-graph (->m (or/c graph? #f))]
           [rendering? (->m boolean?)]
           [get-current-position (->m (and/c real? (>=/c 0)))]
           [get-length (->m (and/c real? positive?))]
           [dump-info (->*m () (string?) void?)]))

;; This submodule stores the member names to the fields for the render% class.
;; Requiring it allows classes to use those fields directly (i.e. for subclasses/mixins.)
;; The exact fields are subject to change.
(module render-fields racket/base
  (require racket/class
           "private/render-settings.rkt")
  (provide (all-defined-out)
           (except-out (all-from-out "private/render-settings.rkt")
                       make-render-settings
                       render-settings?))
  
  (define-local-member-name
    video-graph
    video-sink
    video-start
    video-end
    seek-point
    render-graph
    output-node
    reading-thread
    writing-thread
    stop-rendering-flag
    current-render-settings-lock
    current-render-settings

    feed-buffers-callback-constructor
    feed-buffers
    write-output-callback-constructor
    write-output))

(require 'render-fields)
