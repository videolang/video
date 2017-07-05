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
         graph
         (except-in ffi/unsafe ->)
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
                #:render-mixin (or/c (-> (is-a?/c render<%>) (is-a?/c render<%>)) #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps number?
                #:start (or/c nonnegative-integer? #f)
                #:end (or/c nonnegative-integer? #f)]
               void?)]
 
  ;; Similar to render, however non-blocking.
  ;; Instead, this function immediately returns a channel which will
  ;;   frequently reports the current rebderubg position. Once the renderer
  ;;   has finished, it will put eof signalling that no new input will be
  ;;   placed on the channel.
  [render/async (->* [any/c]
                     [(or/c path-string? path? #f)
                      #:render-mixin (or/c (-> (is-a?/c render<%>) (is-a?/c render<%>)) #f)
                      #:width (and/c integer? positive?)
                      #:height (and/c integer? positive?)
                      #:fps number?
                      #:start (or/c nonnegative-integer? #f)
                      #:end (or/c nonnegative-integer? #f)]
                     async-channel?)])

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
 ;;      (displayln (send r get-position?))
 ;;      (loop)))
 ;; 
 render%

 ;; Interface for render%
 render<%>)

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
        #:destination dest
        #:width width
        #:height height
        #:start start
        #:end end
        #:fps fps)
  (define in-t (thread (λ () (send r feed-buffers))))
  (define out-t (thread (λ () (send r write-output))))
  (thread-wait in-t)
  (thread-wait out-t))

(define (render/async video
                      [dest #f]
                      #:render-mixin [render-mixin #f]
                      #:width [width 1920]
                      #:height [height 1080]
                      #:start [start #f]
                      #:end [end #f]
                      #:fps [fps 25])
  (define channel (make-async-channel))
  (define r% ((or render-mixin values) render%))
  (define r
    (new r%
         [source video]))
  (send r setup
        #:destination dest
        #:width width
        #:height height
        #:start start
        #:end end
        #:fps fps)
  (thread
   (λ ()
     (define in-t (thread (λ () (send r feed-buffers))))
     (define out-t (thread (λ () (send r write-output))))
     (let loop ()
       (when (send r rendering?)
         (async-channel-put channel (send r get-position))
         (sleep 0.01)
         (loop)))
     (thread-wait in-t)
     (thread-wait out-t)
     (async-channel-put channel eof)))
  channel)

(define render<%>
  (interface () copy setup feed-buffers write-output rendering? get-current-position))

(define render%
  (class* object% (render<%>)
    (super-new)
    (init-field source)

    (define video-graph (video:mk-render-graph))
    (define video-sink (parameterize ([video:current-render-graph video-graph])
                         (video:convert source)))

    (define render-graph #f)
    (define output-node #f)

    (define graph-obj #f)
    (define input-bundles #f)
    (define output-bundle #f)

    (define current-render-status (mk-render-status-box))

    ;; Copy this renderer, that way it can be
    ;; setup to multiple profiles
    (define/public (copy)
      (new render% [source source]))

    ;; Setup the renderer to a specific output context.
    ;; This method can be called multiple times, but it is
    ;; NOT THREADSAFE!
    (define/public (setup #:destination [dest #f]
                          #:width [width 1920]
                          #:height [height 1080]
                          #:start [start #f]
                          #:end [end #f]
                          #:fps [fps 25]
                          #:pix-fmt [pix-fmt 'yuv420p]
                          #:sample-fmt [sample-fmt 'fltp]
                          #:sample-rate [sample-rate 44100]
                          #:channel-layout [channel-layout  'stereo])
      (define out-path (path->complete-path (or dest "out.mp4")))
      (set! render-graph (graph-copy video-graph))
      (define trim-node
        (cond [(or start end)
               (define t-node
                 (mk-filter-node
                  (hash 'video (mk-filter
                                "trim" (let* ([r (hash)]
                                              [r (if start (hash-set r "start" start) r)]
                                              [r (if end (hash-set r "end" end) r)])
                                         r))
                        'audio (mk-filter
                                "atrim" (let* ([r (hash)]
                                               [r (if start (hash-set r "start" start) r)]
                                               [r (if end (hash-set r "end" end) r)])
                                          r)))
                  #:counts (hash 'video 1 'audio 1)))
               (add-vertex! render-graph t-node)
               (add-directed-edge! render-graph video-sink t-node 1)
               t-node]
              [else video-sink]))
      (define pad-node
        (mk-filter-node
         (hash 'video (mk-filter "pad"
                                 (hash "width" width
                                       "height" height))
               'audio (mk-filter "anull"))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph pad-node)
      (add-directed-edge! render-graph trim-node pad-node 1)
      (define fps-node
        (mk-filter-node
         (hash 'video (mk-filter "fps"
                                 (hash "fps" fps)))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph fps-node)
      (add-directed-edge! render-graph pad-node fps-node 1)
      (define pix-fmt-node
        (mk-filter-node
         (hash 'video (mk-filter "format" (hash "pix_fmts" pix-fmt))
               'audio (mk-filter "aformat" (hash "sample_fmts" sample-fmt
                                                 "sample_rates" sample-rate
                                                 "channel_layouts" channel-layout)))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph pix-fmt-node)
      (add-directed-edge! render-graph fps-node pix-fmt-node 1)
      (define out-bundle (stream-bundle->file out-path 'vid+aud))
      (define audio-str (dict-ref (stream-bundle-stream-table out-bundle) 'audio))
      (define sink-node
        (mk-sink-node out-bundle
                      #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph sink-node)
      (add-directed-edge! render-graph pix-fmt-node sink-node 1)
      (set! output-node pad-node)
      ;(displayln (graphviz render-graph))
      (let-values ([(g i o) (init-filter-graph render-graph)])
        (set! graph-obj g)
        (set! input-bundles i)
        (set! output-bundle o)))

    ;; Send all of the input pads into the render graph.
    ;; This method sould be run in its own thread
    (define/public (feed-buffers)
      (define threads
        (for/list ([bundle (in-list input-bundles)])
          (thread
           (λ ()
             (demux-stream bundle
                           #:by-index-callback (filtergraph-insert-packet))))))
      (map thread-wait threads))

    ;; Pull Packets out of the render graph as quickly as possible
    ;; This method sould be run in its own thread
    (define/public (write-output)
      (mux-stream output-bundle
                  #:by-index-callback (filtergraph-next-packet
                                       #:render-status current-render-status)))

    (define/public (rendering?)
      (render-status-box-rendering? current-render-status))

    ;; Get the absolute value of current position of the output stream
    ;;   (presuming that a `write-results` is currently in progress. The result does not change
    ;;   if the stream is trimmed.
    ;; If the video is not being rendered it returns an undefined (numeric or #f) result
    ;; Basically, treat this result as something that will occasionally be wrong
    ;;   (like for a progress bar).
    (define/public (get-current-position)
      (render-status-box-position current-render-status))))
