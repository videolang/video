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
         racket/port
         graph
         (except-in ffi/unsafe ->)
         (prefix-in base: racket/base)
         (prefix-in file: file/convertible)
         (only-in pict pict? pict->bitmap)
         racket/async-channel
         "private/init.rkt"
         "private/ffmpeg.rkt"
         "private/ffmpeg-pipeline.rkt"
         (prefix-in ffmpeg: "private/ffmpeg-pipeline.rkt")
         (prefix-in video: "private/video.rkt")
         "private/utils.rkt"
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
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps real?
                #:start (or/c (and/c real? (>=/c 0)) #f)
                #:end (or/c (and/c real? (>=/c 0)) #f)]
               void?)]
 
  ;; Similar to render, however non-blocking.
  ;; Instead, this function immediately returns a channel which will
  ;;   frequently reports the current rebderubg position. Once the renderer
  ;;   has finished, it will put eof signalling that no new input will be
  ;;   placed on the channel.
  [render/async (->* [any/c]
                     [(or/c path-string? path? #f)
                      #:render-mixin (or/c (-> render<%>/c render<%>/c) #f)
                      #:width (and/c integer? positive?)
                      #:height (and/c integer? positive?)
                      #:fps real?
                      #:start (or/c (and/c real? (>=/c 0)) #f)
                      #:end (or/c (and/c real? (>=/c 0)) #f)
                      #:mode (or/c 'verbose #f)]
                     async-channel?)]

  ;; A hybrid of render and render/async. This version blocks until the
  ;;   the video is finished rendering. However, it optionally takes an output
  ;;   port that it writes to. This is useful for having an automatic progress bar
  ;;   sent to the console.
  [render/pretty (->* [any/c]
                      [(or/c path-string? path? #f)
                       #:render-mixin (or/c (-> render<%>/c render<%>/c) #f)
                       #:width (and/c integer? positive?)
                       #:height (and/c integer? positive?)
                       #:fps real?
                       #:start (or/c (and/c real? (>=/c 0)) #f)
                       #:end (or/c (and/c real? (>=/c 0)) #f)
                       #:port (or/c output-port? #f)
                       #:mode (or/c 'verbose 'silent #f)]
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
                             #:speed real?)
                            render-settings?)])

 ;; Interface for render%
 render<%>
 render<%>/c)

(define (render video
                [dest #f]
                #:render-mixin [render-mixin #f]
                #:width [width 1920]
                #:height [height 1080]
                #:start [start #f]
                #:end [end #f]
                #:fps [fps 25])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define r% ((or render-mixin values) render%))
  (define r
    (new r%
         [source video]))
  (send r setup
        (make-render-settings #:destination dest*
                              #:width width
                              #:height height
                              #:start start
                              #:end end
                              #:fps fps))
  (send r start-rendering #t))

(define (render/async video
                      [dest #f]
                      #:render-mixin [render-mixin #f]
                      #:width [width 1920]
                      #:height [height 1080]
                      #:start [start #f]
                      #:end [end #f]
                      #:fps [fps 25]
                      #:mode [mode #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define channel (make-async-channel))
  (define r% ((or render-mixin values) render%))
  (define r
    (new r%
         [source video]))
  (send r setup
        (make-render-settings #:destination dest*
                              #:width width
                              #:height height
                              #:start start
                              #:end end
                              #:fps fps))
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
  channel)

(define (render/pretty video
                       [dest #f]
                       #:render-mixin [render-mixin #f]
                       #:width [width 1920]
                       #:height [height 1080]
                       #:start [start #f]
                       #:end [end #f]
                       #:fps [fps 25]
                       #:port [port* (current-output-port)]
                       #:mode [mode #f])
  (define port (or (and (not (eq? mode 'silent)) port*)
                   (open-output-nowhere)))
  (define channel
    (render/async video dest
                  #:render-mixin render-mixin
                  #:width width
                  #:height height
                  #:start start
                  #:end end
                  #:fps fps
                  #:mode (if (eq? mode 'silent) #f mode)))
  (when (eq? mode 'verbose)
    (displayln (graphviz (async-channel-get channel))))
  (let loop ()
    (define pos (async-channel-get channel))
    (unless (eof-object? pos)
      (fprintf port "\r~a" (real->decimal-string pos))
      (loop)))
  (newline port))

;; Defined in a submodule so that classes
;; extending this one can make use of it.
(module render-settings racket/base
  (provide (all-defined-out))
  (struct render-settings (destination
                           width
                           height
                           start
                           end
                           fps
                           format
                           video-codec
                           audio-codec
                           subtitle-codec
                           pix-fmt
                           sample-fmt
                           sample-rate
                           channel-layout
                           speed))
  (define (make-render-settings #:destination [d #f]
                                #:width [w 1920]
                                #:height [h 1080]
                                #:start [s #f]
                                #:end [e #f]
                                #:fps [f 25]
                                #:format [fo #f]
                                #:video-codec [vc #f]
                                #:audio-codec [ac #f]
                                #:subtitle-codec [sc #f]
                                #:pix-fmt [pf 'yuv420p]
                                #:sample-fmt [sf 'fltp]
                                #:sample-rate [sr 44100]
                                #:channel-layout [cl 'stereo]
                                #:speed [sp 1])
    (render-settings d w h s e f fo vc ac sc pf sf sr cl sp)))
(require 'render-settings)

(define render%
  (class object%
    (super-new)
    (init-field source)

    (field [video-graph #f]
           [video-sink #f]
           [render-graph #f]
           [output-node #f]
           [video-start 0]
           [video-end #f])
    
    (define graph-obj #f)
    (define input-bundles #f)
    (define output-bundles #f)

    (define current-render-status (mk-render-status-box))

    (field [running-lock (make-semaphore 1)]
           [reading-thread #f]
           [writing-thread #f])


    (define current-render-settings-lock (make-semaphore 1))
    (define current-render-settings (make-render-settings))

    ;; Flag states:
    ;;   - running : Actively reading/writing
    ;;   - stopping : Actively reading/writing, requested to stop asap
    ;;   - stopped : No longer reading/writing
    ;; Must always use call-with-semaphore before modifying the status flag.
    (define stop-reading-flag 'stopped)
    (define reading-stopped-signal (make-async-channel))
    (define stop-reading-semaphore (make-semaphore 1))
    (define stop-writing-flag 'stopped)
    (define writing-stopped-signal (make-async-channel))
    (define stop-writing-semaphore (make-semaphore 1))

    ;; Copy this renderer, that way it can be
    ;; setup to multiple profiles
    (define/public (copy)
      (new render% [source source]))

    ;; Setup the renderer to a specific output context.
    ;; This method can be called multiple times, but it is
    ;; NOT THREADSAFE!
    (define/public (setup settings)
      (call-with-semaphore
       current-render-settings-lock
       (λ ()
         (set! video-graph (video:mk-render-graph))
         (set! video-sink (parameterize ([video:current-render-graph video-graph])
                            (video:convert source)))
         (set! current-render-settings settings)
         (match settings
           [(struct* render-settings ([destination dest]
                                      [width width]
                                      [height height]
                                      [start start*]
                                      [end end*]
                                      [fps fps]
                                      [format format]
                                      [video-codec video-codec]
                                      [audio-codec audio-codec]
                                      [pix-fmt pix-fmt]
                                      [sample-fmt sample-fmt]
                                      [sample-rate sample-rate]
                                      [channel-layout channel-layout]
                                      [speed speed]))
            (define extension
              (match format
                ['raw "raw"]
                [_ "mp4"]))
            ;; XXX We're going to need two bundles here.
            ;; AKA, the renderer is going to need to jugle
            ;;   two different muxers.
            (define bundle-spec
              (match format
                ['raw 'video]
                [_ 'vid+aud]))
            (define format-name
              (match format
                ['raw "rawvideo"]
                [_ #f]))
            (define video-streams 1)
            (define audio-streams
              (match format
                ['raw 0]
                [_ 1]))
            (define out-path (path->complete-path (or dest (base:format "out.~a" extension))))
            (set! render-graph (graph-copy video-graph))
            (define start-node
              (mk-fifo-node
               #:props (node-props video-sink)
               #:counts (hash 'video video-streams 'audio audio-streams)))
            (add-vertex! render-graph start-node)
            (add-directed-edge! render-graph video-sink start-node 1)
            (define start (or start* (dict-ref (node-props start-node) "start" 0)))
            (define end (or end* (dict-ref (node-props start-node) "end" 0)))
            (define trim-node
              (mk-filter-node
               (hash 'video (mk-filter
                             "trim" (let* ([r (hash)]
                                           [r (if start
                                                  (hash-set r "start" (racket->ffmpeg start))
                                                  r)]
                                           [r (if end (hash-set r "end" (racket->ffmpeg end)) r)])
                                      r))
                     'audio (mk-filter
                             "atrim" (let* ([r (hash)]
                                            [r (if start
                                                   (hash-set r "start" (racket->ffmpeg start))
                                                   r)]
                                            [r (if end (hash-set r "end" (racket->ffmpeg end)) r)])
                                       r)))
               #:props (node-props video-sink)
               #:counts (node-counts start-node)))
            (add-vertex! render-graph trim-node)
            (add-directed-edge! render-graph start-node trim-node 1)
            (define pad-node
              (mk-filter-node
               (hash 'video (mk-filter "scale"
                                       (hash "width" width
                                             "height" height))
                     'audio (mk-filter "anull"))
               #:props (node-props video-sink)
               #:counts (node-counts trim-node)))
            (add-vertex! render-graph pad-node)
            (add-directed-edge! render-graph trim-node pad-node 1)
            (define fps-node
              (mk-filter-node
               (hash 'video (mk-filter "fps"
                                       (hash "fps" fps)))
               #:props (node-props video-sink)
               #:counts (node-counts trim-node)))
            (add-vertex! render-graph fps-node)
            (add-directed-edge! render-graph pad-node fps-node 1)
            (define pix-fmt-node
              (mk-filter-node
               (hash 'video (mk-filter "format" (hash "pix_fmts" pix-fmt))
                     'audio (mk-filter "aformat" (hash "sample_fmts" sample-fmt
                                                       "sample_rates" sample-rate
                                                       "channel_layouts" channel-layout)))
               #:props (node-props video-sink)
               #:counts (node-counts trim-node)))
            (add-vertex! render-graph pix-fmt-node)
            (add-directed-edge! render-graph fps-node pix-fmt-node 1)
            (define speed-node
              (cond
                [(= speed 0)
                 pix-fmt-node]
                [else
                 (define speed-node
                   (mk-filter-node
                    (hash 'video (mk-filter
                                  "setpts"
                                  (hash "expr" (base:format "(PTS-STARTPTS)*~a"
                                                            (exact->inexact (/ 1 (abs speed))))))
                          'audio (mk-filter "asetrate"
                                            (hash "r" (exact->inexact (abs (* sample-rate speed))))))
                    #:props (node-props video-sink)
                    #:counts (node-counts trim-node)))
                 (add-vertex! render-graph speed-node)
                 (add-directed-edge! render-graph pix-fmt-node speed-node 1)
                 speed-node]))
            (define drop-node
              (match video-codec
                ['mpeg1video
                 (define drop-node
                   (mk-filter-node
                    (hash 'video (mk-filter "select"
                                            (hash "expr" (format "'not(mod(n\\,~a)'"
                                                                 (exact-ceiling speed))))
                    #:props (node-props video-sink)
                    #:counts (node-counts trim-node))))
                 (add-vertex! render-graph drop-node)
                 (add-directed-edge! render-graph speed-node drop-node 1)
                 drop-node]
                [_ speed-node]))
            (define rev-node
              (cond
                [(< speed 0)
                 (define rev-node
                   (mk-filter-node
                    (hash 'video (mk-filter "reverse")
                          'audio (mk-filter "areverse"))
                    #:props (node-props video-sink)
                    #:counts (node-counts trim-node)))
                 (add-vertex! render-graph rev-node)
                 (add-directed-edge! render-graph drop-node rev-node 1)
                 rev-node]
                [else drop-node]))
            (define out-bundle
              (stream-bundle->file out-path bundle-spec
                                   #:format-name format-name))
            (define sink-node
              (mk-sink-node out-bundle
                            #:counts (node-counts trim-node)))
            (add-vertex! render-graph sink-node)
            (add-directed-edge! render-graph rev-node sink-node 1)
            (set! output-node pad-node)
            (set! video-start (video:get-property video-sink "start"))
            (set! video-end (video:get-property video-sink "end"))
            ;(displayln (graphviz render-graph))
            (let-values ([(g i o) (init-filter-graph render-graph)])
              (set! graph-obj g)
              (set! input-bundles i)
              (set! output-bundles o))]))))

    (define/public (feed-buffers-callback-constructor)
      (filtergraph-insert-packet))

    ;; Send all of the input pads into the render graph.
    ;; This method sould be run in its own thread
    (define/public (feed-buffers)
      (define currently-running?
        (call-with-semaphore
         stop-reading-semaphore
         (λ ()
           (define ret (eq? stop-reading-flag 'running))
           (set! stop-reading-flag 'running)
           ret)))
      (when currently-running?
        (error 'feed-buffers "Reader already running"))
      (define threads
        (for/list ([bundle (in-list input-bundles)])
          (thread
           (λ ()
             (define callback (feed-buffers-callback-constructor))
             (define demux (new demux%
                                [bundle bundle]
                                [by-index-callback callback]))
             (send demux init)
             (let loop ()
               (when (call-with-semaphore
                      stop-reading-semaphore
                      (λ () (eq? stop-reading-flag 'running)))
                 (define got-data?
                   (send demux wait-for-packet-need 1))
                 (cond [(not got-data?) (loop)]
                       [(send demux read-packet) (loop)]
                       [else (void)])))
             (send demux close)))))
      (for-each thread-wait threads)
      (call-with-semaphore
       stop-reading-semaphore
       (λ ()
         (when (eq? stop-reading-flag 'stopping)
           (async-channel-put reading-stopped-signal 'stopped))
         (set! stop-reading-flag 'stopped)))
      (void))

    ;; Asynchronous thread to start-rendering.
    ;; Simply calls `feed-buffers` and `write-output` (protected
    ;;   methods) in different threads. The `rendering` method
    ;;   determines when those threads stop.
    ;; When wait is true, equivalent to calling `wait-for-rendering`
    ;; after this method concludes.
    ;; Boolean -> Void
    (define/public (start-rendering [wait? #f])
      (call-with-semaphore
       running-lock
       (λ ()
         (set! reading-thread (thread (λ () (feed-buffers))))
         (set! writing-thread (thread (λ () (write-output))))))
      (when wait?
        (wait-for-rendering)))

    ;; Wait for rendering to finish, then return.
    (define/public (wait-for-rendering)
      (call-with-semaphore
       running-lock
       (λ ()
         (and writing-thread (thread-wait writing-thread))
         (and reading-thread (thread-wait reading-thread)))))

    ;; Used for early termination of `feed-buffers`.
    ;; This is needed because trimmed outputs will
    ;;   NEVER finish reading from the inputs.
    ;; Will not return until program is in a stopped state.
    ;;
    ;;
    ;; WARNING: While this method _is_ threadsafe, it is
    ;;   possible to start re-running before thread begins again, depending
    ;;   on your use of the other methods.
    (define/public (stop-rendering)
      (let loop ()
        (define prev-status
          (call-with-semaphore
           stop-writing-semaphore
           (λ ()
             (define ret stop-writing-flag)
             (when (eq? stop-writing-flag 'running)
               (set! stop-writing-flag 'stopping))
             ret)))
        (match prev-status
          ['stopped (void)]
          ['stopping
           (sleep 1)
           (loop)]
          ['running
           (async-channel-get writing-stopped-signal)
           (void)]))
      (wait-for-rendering))

    (define/public (write-output-callback-constructor #:render-status render-status)
      (filtergraph-next-packet #:render-status render-status))

    ;; Pull Packets out of the render graph as quickly as possible
    ;; This method sould be run in its own thread
    (define/public (write-output)
      (define currently-stopped?
        (call-with-semaphore
         stop-writing-semaphore
         (λ ()
           (define ret (eq? stop-writing-flag 'stopped))
           (set! stop-writing-flag 'running)
           ret)))
      (unless currently-stopped?
        (error 'write-output "Already writing output"))
      (define threads
        (for/list ([output-bundle (in-list output-bundles)])
          (thread
           (λ ()
             (define proc (write-output-callback-constructor #:render-status current-render-status))
             (define mux (new mux%
                              [bundle output-bundle]
                              [by-index-callback proc]))
             (send mux init)
             (send mux open)
             (send mux write-header)
             (let loop ()
               (when (call-with-semaphore stop-writing-semaphore
                                          (λ () (eq? stop-writing-flag 'running)))
                 (define out (send mux write-packet))
                 (define continue?
                   (and out
                       #;(and (= out 0)
                            (call-with-semaphore stop-reading-semaphore
                                                 (λ () (eq? stop-reading-flag 'stopped))))))
                 (when continue?
                   (loop))))
             (send mux write-trailer)
             (send mux close)))))
      (map thread-wait threads)
      (define wait-for-reading?
        (call-with-semaphore
         stop-reading-semaphore
         (λ ()
           (define reading? (eq? stop-reading-flag 'running))
           (when reading?
             (set! stop-reading-flag 'stopping))
           reading?)))
      (when wait-for-reading?
        (async-channel-get reading-stopped-signal))
      (call-with-semaphore
       stop-writing-semaphore
       (λ ()
         (when (eq? stop-writing-flag 'stopping)
           (async-channel-put writing-stopped-signal 'stopped)
           (set! stop-writing-flag 'stopped))))
      (void))

    ;; Seek to a specific position (in seconds). Convience method
    ;;    whose functionality can be duplicated entirely from other methods.
    (define/public (seek position)
      (stop-rendering)
      (setup (struct-copy render-settings current-render-settings
                          [start position]))
      (start-rendering))

    ;; Convience method to resize (in pixels) the video. Can be duplicated
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
      (call-with-semaphore
       running-lock
       (λ ()
         (or (and reading-thread (thread-running? reading-thread))
             (and writing-thread (thread-running? writing-thread))))))
       
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
      (+ video-start (render-status-box-position current-render-status)))

    ;; Return the length of the internal video (not the setup length) in seconds
    (define/public (get-length)
      (and video-end video-start
           (- video-end video-start)))))

(define render<%>
  (class->interface render%))

;; Prototype contract for render% classes.
(define render<%>/c
  (class/c [copy (->m (instanceof/c (recursive-contract render<%>/c)))]
           [setup (->m render-settings? void?)]
           [start-rendering (->*m () (boolean?) void?)]
           [wait-for-rendering (->m void?)]
           [stop-rendering (->m void?)]
           [seek (->m (and/c real? (>=/c 0)) void?)]
           [resize (->m (and/c real? positive?) (and/c real? positive?) void?)]
           [set-speed (->m real? void?)]
           [get-video-graph (->m graph?)]
           [get-render-graph (->m (or/c graph? #f))]
           [rendering? (->m boolean?)]
           [get-current-position (->m (and/c real? (>=/c 0)))]
           [get-length (->m (and/c real? positive?))]))

;; This submodule stores the member names to the fields for the render% class.
;; Requiring it allows classes to use those fields directly (i.e. for subclasses/mixins.)
;; The exact fields are subject to change.
(module render-fields racket/base
  (require racket/class
           (submod ".." render-settings))
  (provide (all-defined-out)
           (except-out (all-from-out (submod ".." render-settings))
                       make-render-settings
                       render-settings?))
  
  (define-local-member-name
    video-graph
    video-sink
    video-start
    video-end
    render-graph
    output-node
    running-lock
    reading-thread
    writing-thread
    current-render-settings-lock
    current-render-settings

    feed-buffers-callback-constructor
    feed-buffers
    write-output-callback-constructor
    write-output))
(require 'render-fields)
