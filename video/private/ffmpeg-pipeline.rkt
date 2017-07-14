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

(require racket/match
         ffi/unsafe
         racket/list
         racket/set
         racket/string
         racket/dict
         racket/async-channel
         racket/function
         racket/struct
         racket/draw
         racket/class
         racket/format
         (except-in racket/contract ->)
         (prefix-in con: racket/contract)
         (prefix-in base: racket/base)
         racket/system
         racket/math
         graph
         "init.rkt"
         "packetqueue.rkt"
         "ffmpeg.rkt"
         "threading.rkt")
(provide (all-defined-out))

(define DEFAULT-WIDTH 1920)
(define DEFAULT-HEIGHT 1080)
(define DEFAULT-FPS 25)
(define DEFAULT-MAX-B-FRAMES 8)
(define DEFAULT-SAMPLE-FREQ 44100)
(define DEFAULT-PIX-FMT 'yuv420p)
(define DEFAULT-SAMPLE-FMT 'fltp)
(define DEFAULT-CHANNEL-LAYOUT 'stereo)

;; This is the location of the ffmpeg path ONLY IF it
;;  is installed on the computer. This must only be used
;;  for debug purposes.
(define ffmpeg (find-executable-path "ffmpeg"))

;; ===================================================================================================

(struct stream-bundle (raw-streams
                       streams
                       stream-table
                       avformat-context
                       options-dict
                       file)
  #:mutable)
(define (mk-stream-bundle #:raw-streams [rs #'()]
                          #:streams [s #'()]
                          #:stream-table [st (make-hash)]
                          #:avformat-context [ctx #f]
                          #:options-dict [o #f]
                          #:file [f #f])
  (stream-bundle rs s st ctx o f))
(define (fill-stream-bundle bundle [use-type 'encode])
  (define table (make-hash))
  (for ([s (in-list (stream-bundle-streams bundle))]
        [index (in-naturals)])
    (match s
      [(struct* codec-obj ([id id]
                           [type type]))
       (define codec ((match use-type
                        ['encode avcodec-find-encoder]
                        ['decode avcodec-find-decoder])
                      id))
       (set-codec-obj-codec! s codec)
       (set-codec-obj-index! s index)
       (set-codec-obj-codec-context! s (avcodec-alloc-context3 codec))
       (dict-set! table type s)]))
  (set-stream-bundle-stream-table! bundle table)
  bundle)

(struct codec-obj (codec-parameters
                   type
                   index
                   id
                   codec
                   codec-context
                   stream
                   pts
                   next-pts
                   dts
                   next-dts
                   buffer
                   buffer-context
                   callback-data
                   extra-parameters
                   start-time
                   start-offset
                   flags)
  #:mutable)
(define (mk-codec-obj #:codec-parameters [occ #f]
                      #:type [t #f]
                      #:index [i #f]
                      #:id [id #f]
                      #:codec [codec #f]
                      #:codec-context [codec-context #f]
                      #:stream [s #f]
                      #:pts [p 0]
                      #:next-pts [np 0]
                      #:dts [d 0]
                      #:next-dts [nd 0]
                      #:buffer [bf #f]
                      #:buffer-context [bc #f]
                      #:callback-data [cd #f]
                      #:extra-parameters [ep (mk-extra-codec-parameters)]
                      #:start-time [st (current-inexact-milliseconds)]
                      #:start-offset [so #f]
                      #:flags [f '()])
  (codec-obj occ t i id codec codec-context s p np d nd bf bc cd ep st #f f))

(struct extra-codec-parameters (time-base
                                gop-size
                                pix-fmt
                                color-range
                                sample-fmt
                                max-b-frames))
(define (mk-extra-codec-parameters #:time-base [tb #f]
                                   #:gop-size [gs #f]
                                   #:pix-fmt [pix-fmt #f]
                                   #:color-range [color-range #f]
                                   #:sample-fmt [sample-fmt #f]
                                   #:max-b-frames [max-b-frames #f])
  (extra-codec-parameters tb gs pix-fmt color-range sample-fmt max-b-frames))

(struct render-status-box (position
                           rendering?)
  #:mutable)
(define (mk-render-status-box #:position [p 0]
                              #:rendering? [r? #f])
  (render-status-box p r?))

(struct filter (name
                args
                instance-name)
  #:transparent)
(define (mk-filter name
                   [args (hash)]
                   #:instance-name [instance-name #f])
  (filter name args instance-name))
(define (filter->string filter)
  (string-append (filter-name filter)
                 (if (filter-instance-name filter)
                     (format "@~a" filter-instance-name)
                     "")
                 (if (dict-empty? (filter-args filter)) "" "=")
                 (string-join
                  (for/list ([(k v) (in-dict (filter-args filter))])
                    (let loop ([v v])
                      (define v*
                        (cond
                          [(list? v) (string-join (map loop v) ";")]
                          [(interval? v) (interval->string v)]
                          [else v]))
                      (if (string? k)
                          (format "~a=~a" k v*)
                          (format "~a" v*))))
                  ":")))
(define (filter->avfilter f graph)
  (define filter (avfilter-get-by-name (filter-name f)))
  (define filter-ctx (avfilter-graph-alloc-filter graph filter (filter-instance-name f)))
  (for ([(k v) (in-dict (filter-args f))])
    (define set-proc
      (cond
        [(exact-integer? v) av-opt-set-int]
        [(and (exact? v) (rational? v)) av-opt-set-q]
        [(string? v) av-opt-set]
        [else (error 'filter->filter "unkown type for ~a" v)]))
    (set-proc filter-ctx k v '(search-children)))
  (avfilter-init-str filter-ctx #f))

(struct command (flags target command arg))
(define (mk-command target command
                    #:flags [flags #f]
                    #:arg [arg #f])
  (command flags target command arg))
(define (command->string com)
  (match com
    [(struct* command ([flags flags]
                       [target target]
                       [command command]
                       [arg arg]))
     (string-append (if flags
                        (format "[~a] "
                                (match flags
                                  ['(enter) "enter"]
                                  ['(leave) "leave"]
                                  [_ "enter|leave"]))
                        "")
                    target
                    " "
                    command
                    (if arg (format " ~a" arg) ""))]))

(struct interval (start end commands))
(define (mk-interval start commands
                     #:end [end #f])
  (interval start end commands))
(define (interval->string int)
  (match int
    [(struct* interval ([start start]
                        [end end]
                        [commands commands]))
     (string-append start
                    (if end (format "-~a" end) "")
                    " "
                    (string-join (map command->string commands) ","))]))

;; ===================================================================================================

;; A callback table is:
;; (Dictof (U 'video 'audio 'subtitle 'data 'attchment) -> Proc)

(define (callback-ref tab k)
  (if tab
      (dict-ref tab k (λ () empty-proc))
      empty-proc))
(define (empty-proc mode obj packet)
  (when packet
    (av-packet-unref packet)))

;; (U av-dictionary Hash #f) -> av-dictionary
(define (convert-dict dict)
  (cond
    [(hash? dict)
     (define ret #f)
     (for ([(k v) (in-hash dict)])
       (set! ret (av-dict-set ret k v 0)))
     ret]
    [else dict]))

(define (maybe-av-find-best-stream avformat type)
  (with-handlers ([exn:ffmpeg:stream-not-found? (λ (e) #f)])
    (av-find-best-stream avformat type -1 -1 #f 0)))

;; ===================================================================================================

(define (file->stream-bundle file)
  (define avformat (avformat-open-input file #f #f))
  (avformat-find-stream-info avformat #f)
  (define raw-strs (avformat-context-streams avformat))
  (define stream-table (make-hash))
  (define streams
    (for/vector ([i raw-strs]
                 [index (in-naturals)])
      (define codec-parameters (avstream-codecpar i))
      (define codec-name (avcodec-parameters-codec-type codec-parameters))
      (define codec-id (avcodec-parameters-codec-id codec-parameters))
      (define codec (avcodec-find-decoder codec-id))
      (define codec-ctx (avcodec-parameters-to-context codec codec-parameters))
      (when (eq? codec-name 'video)
        (set-avcodec-context-time-base! codec-ctx (avstream-time-base i))
        (set-avcodec-context-framerate! codec-ctx (av-guess-frame-rate avformat i #f)))
      (avcodec-open2 codec-ctx codec #f)
      (define obj (mk-codec-obj #:codec-parameters codec-parameters
                                #:type codec-name
                                #:index index
                                #:stream i
                                #:id codec-id
                                #:codec codec
                                #:codec-context codec-ctx))
      (dict-set! stream-table codec-name obj)
      obj))
  (mk-stream-bundle #:raw-streams raw-strs
                    #:avformat-context avformat
                    #:streams streams
                    #:stream-table stream-table
                    #:file file))

(define demux%
  (class object%
    (init-field bundle
                [callback-table (hash)]
                [by-index-callback #f])
    (super-new)
    
    (define avformat (stream-bundle-avformat-context bundle))
    (define streams (stream-bundle-streams bundle))
    (define stream-table (stream-bundle-stream-table bundle))
    
    (define/public (dump-info [testfile #f])
      (av-dump-format avformat 0 testfile 0))

    ;; Takes an optional timeout in seconds. Returns true if data is found
    ;;   or false if timed out (if timeout provided).
    ;; (U Number #f) -> Boolean
    (define/public (wait-for-packet-need [timeout #f])
      (define start-time (current-inexact-milliseconds))
      (let loop ()
        (define needed?
          (for/fold ([needed? #f])
                    ([str (stream-bundle-streams bundle)])
            (define buff-ctx (codec-obj-buffer-context str))
            (cond
              [needed? needed?]
              [buff-ctx
               (> (av-buffersrc-get-nb-failed-requests buff-ctx) 0)]
              [else #t])))
        (define run-time (- (current-inexact-milliseconds) start-time))
        (define timed-out? (and timeout (> (/ run-time 1000) timeout)))
        (cond [(or needed? timed-out?)
               (sleep 0.01)
               (loop)]
              [else (not timed-out?)])))
    
    (define/public (init)
      (for ([i streams])
        (match i
          [(struct* codec-obj ([type codec-name]))
           (when (and (not by-index-callback) (hash-ref stream-table codec-name #f))
             (error 'decoder-stream "Stream type ~a already present" codec-name))
           (hash-set! stream-table codec-name i)
           (when by-index-callback
             (by-index-callback 'init i #f))]))
      (unless by-index-callback
        (for ([(k v) (in-hash stream-table)])
          ((callback-ref callback-table k)) 'init v #f))
      (for  ([i streams])
        (match i
          [(struct* codec-obj ([start-offset start-offset]
                               [stream stream]
                               [index index]))
           (when start-offset
             (av-seek-frame avformat
                            index
                            (exact-floor (/ (max 0 (- start-offset 1))
                                            (avstream-time-base stream)))
                            '()))])))

    ;; #t - More to read
    ;; #f - Done reading
    (define/public (read-packet)
      (define packet (av-read-frame avformat))
      (cond
        [packet
         (define index (avpacket-stream-index packet))
         (define obj (vector-ref streams index))
         (cond [by-index-callback (by-index-callback 'loop obj packet)]
               [else
                (define type (codec-obj-type obj))
                (when (eq? obj (hash-ref stream-table type))
                  ((callback-ref callback-table type) 'loop obj packet))])
         (av-packet-free packet)
         #t]
        [else #f]))
    
    (define/public (close)
      (unless by-index-callback
        (for ([(k v) (in-hash stream-table)])
          ((callback-ref callback-table k)) 'close v #f))
      (for ([i (in-vector streams)])
        (match i
          [(struct* codec-obj ([codec-parameters codec-parameters]
                               [codec-context codec-context]
                               [index index]))
           (when by-index-callback
             (by-index-callback 'close i #f))
           (unless (set-member? (codec-obj-flags i) 'no-close-context)
             (avcodec-close codec-context))]))
      (avformat-close-input avformat))))

;; Callback ops:
;;   'init
;;   'loop
;;   'close
(define (demux-stream bundle
                      #:callback-table [callback-table (hash)]
                      #:by-index-callback [by-index-callback #f])
  (define demux (new demux%
                     [bundle bundle]
                     [callback-table callback-table]
                     [by-index-callback by-index-callback]))
  (send demux init)
  ;; Main Loop
  (let loop ()
    (send demux wait-for-packet-need)
    (when (send demux read-packet)
      (loop)))
  (send demux close))

;; ===================================================================================================

(define (default-video-parameters format [stream #f])
  (define parameters (avcodec-parameters-alloc))
  (set-avcodec-parameters-codec-id! parameters (av-output-format-video-codec format))
  (set-avcodec-parameters-codec-type! parameters 'video)
  (set-avcodec-parameters-bit-rate! parameters 400000)
  (set-avcodec-parameters-width! parameters 1920)
  (set-avcodec-parameters-height! parameters 1080)
  #;
  (when stream
    (set-avstream-time-base! stream 1/25))
  (define rest
    (mk-extra-codec-parameters #:time-base 1/25
                               #:gop-size 12
                               #:pix-fmt 'yuv420p
                               #:color-range 'mpeg
                               #:max-b-frames 8))
  (values parameters rest))

(define (default-audio-parameters format [stream #f])
  (define parameters (avcodec-parameters-alloc))
  (set-avcodec-parameters-codec-id! parameters (av-output-format-audio-codec format))
  (set-avcodec-parameters-codec-type! parameters 'audio)
  (set-avcodec-parameters-bit-rate! parameters 64000)
  (set-avcodec-parameters-sample-rate! parameters DEFAULT-SAMPLE-FREQ)
  (set-avcodec-parameters-channel-layout! parameters DEFAULT-CHANNEL-LAYOUT)
  (set-avcodec-parameters-channels!
   parameters (av-get-channel-layout-nb-channels DEFAULT-CHANNEL-LAYOUT))
  #;
  (when stream
    (set-avstream-time-base! stream 1/44100))
  (define rest
    (mk-extra-codec-parameters ;#:time-base 1/44100
                               #:sample-fmt 'fltp))
  (values parameters rest))

(define (stream-bundle->file file bundle/spec
                             #:output-format [output-format #f]
                             #:format-name [format-name #f]
                             #:options-dict [options-dict #f])
  (unless (or file output-format format-name)
    (error 'stream-bundle->file "No File, output-format or format-name specified"))
  (define stream-table (make-hash))
  (define streams
    (match bundle/spec
      [(struct* stream-bundle ([streams streams]))
       (for/vector ([i streams]
                    [index (in-naturals)])
         (define ret
           (match i
             [(struct* codec-obj ([type t]
                                  [id i]
                                  [index index]
                                  [codec-parameters parameters]
                                  [callback-data callback-data]))
              (mk-codec-obj #:type t
                            #:id i
                            #:codec-parameters parameters
                            #:index index
                            #:callback-data callback-data)]
             [x (mk-codec-obj #:type x
                              #:index index)]))
         (dict-set! stream-table (codec-obj-type ret) ret)
         ret)]
      [(or 'vid 'video)
       (define video
         (mk-codec-obj #:type 'video
                       #:index 0))
       (dict-set! stream-table 'video video)
       (vector video)]
      [(or 'aud 'audio)
       (define audio
         (mk-codec-obj #:type 'audio
                       #:index 0))
       (dict-set! stream-table 'audio audio)
       (vector audio)]
      [(or 'vid+aud 'video+audio 'movie)
       (define video
         (mk-codec-obj #:type 'video
                       #:index 0))
       (dict-set! stream-table 'video video)
       (define audio
         (mk-codec-obj #:type 'audio
                       #:index 1))
       (dict-set! stream-table 'audio audio)
       (vector video audio)]))
  (define output-context
    (avformat-alloc-output-context2 output-format format-name file))
  (define format (avformat-context-oformat output-context))
  (define video-codec (av-output-format-video-codec format))
  (define audio-codec (av-output-format-audio-codec format))
  (define subtitle-codec (av-output-format-subtitle-codec format))
  (for ([i streams])
    (match i
      [(struct* codec-obj ([type type]
                           [id id]
                           [index index]))
       (define type-codec-id
         (match type
           ['video video-codec]
           ['audio audio-codec]
           ['subtitle subtitle-codec]
           [else #f]))
       (define codec-id (or id type-codec-id))
       (define codec
         (avcodec-find-encoder codec-id))
       (set-codec-obj-codec! i codec)
       (define str (avformat-new-stream output-context #f))
       (set-codec-obj-stream! i str)
       (set-codec-obj-id! i codec-id)
       (set-codec-obj-index! i index)
       (set-avstream-id! str (sub1 (avformat-context-nb-streams output-context)))
       (define maybe-extra-params #f)
       (unless (codec-obj-codec-parameters i)
         (set-codec-obj-codec-parameters!
          i (match (codec-obj-type i)
              ['video
               (define-values (params extra) (default-video-parameters format str))
               (set! maybe-extra-params extra)
               params]
              ['audio
               (define-values (params extra) (default-audio-parameters format str))
               (set! maybe-extra-params extra)
               params]
              [_ #f])))
       (define ctx (avcodec-parameters-to-context codec (codec-obj-codec-parameters i)))
       (set-codec-obj-codec-context! i ctx)
       (when maybe-extra-params
         (define (maybe-set! setter getter)
           (when (getter maybe-extra-params)
             (setter ctx (getter maybe-extra-params))))
         (maybe-set! set-avcodec-context-time-base! extra-codec-parameters-time-base)
         (maybe-set! set-avcodec-context-gop-size! extra-codec-parameters-gop-size)
         (maybe-set! set-avcodec-context-pix-fmt! extra-codec-parameters-pix-fmt)
         (maybe-set! set-avcodec-context-color-range! extra-codec-parameters-color-range)
         (maybe-set! set-avcodec-context-sample-fmt! extra-codec-parameters-sample-fmt)
         (maybe-set! set-avcodec-context-max-b-frames! extra-codec-parameters-max-b-frames))]))
  (mk-stream-bundle #:avformat-context output-context
                    #:options-dict options-dict
                    #:file file
                    #:streams streams
                    #:raw-streams streams
                    #:stream-table stream-table))

(define mux%
  (class object%
    (init-field [bundle bundle]
                [callback-table callback-table]
                [by-index-callback by-index-callback])
    (super-new)
    
    (define output-context
      (stream-bundle-avformat-context bundle))
    (define file (stream-bundle-file bundle))
    (define format (avformat-context-oformat output-context))
    (define streams (stream-bundle-streams bundle))
    
    (define/public (init)
      (define options (convert-dict (stream-bundle-options-dict bundle)))
      ;; Get streams
      (define stream-table (make-hash))
      ;; Get codec and other attributes of decoded video
      (for ([i (in-vector streams)]
            [index (in-naturals)])
        (match i
          [(struct* codec-obj ([id id]
                               [type type]
                               [codec-context ctx]))
           (if by-index-callback
               (by-index-callback 'init i)
               ((dict-ref callback-table type empty-proc) 'init i))
           (when (set-member? (avformat-context-flags output-context) 'globalheader)
             (set-avcodec-context-flags!
              ctx (set-add (avcodec-context-flags ctx) 'global-header)))])))
    
    (define/public (open)
      (define output-context
        (stream-bundle-avformat-context bundle))
      (define options (convert-dict (stream-bundle-options-dict bundle)))
      (for ([i (in-vector streams)])
        (match i
          [(struct* codec-obj ([type type]
                               [codec codec]
                               [codec-context ctx]
                               [stream stream]))
           (define str-opt (av-dict-copy options '()))
           (avcodec-open2 ctx codec str-opt)
           (av-dict-free str-opt)
           ;(avcodec-parameters-from-context (avstream-codecpar stream) ctx)
           (if by-index-callback
               (by-index-callback 'open i)
               ((dict-ref callback-table empty-proc) 'open i))
           (avcodec-parameters-from-context (avstream-codecpar stream) ctx)]))
      ;; Create file.
      ;(av-dump-format output-context 0 file 'output)
      (unless (set-member? (av-output-format-flags format) 'nofile)
        (set-avformat-context-pb!
         output-context (avio-open (avformat-context-pb output-context) file 'write))))
  
    (define remaining-streams (mutable-set))
    (define/public (write-header)
      (avformat-write-header output-context #f)
      (for ([i (in-vector streams)])
        (set-add! remaining-streams i)))
    
    ;; #t - More packets to write
    ;; #f - No more packets to write
    (define/public (write-packet)
      (cond [(set-empty? remaining-streams) #f]
            [else
             (define min-stream
               (for/fold ([min-stream #f])
                         ([i (in-set remaining-streams)])
                 (if min-stream
                     (match (av-compare-ts (codec-obj-next-pts min-stream)
                                           (avstream-time-base (codec-obj-stream min-stream))
                                           (codec-obj-next-pts i)
                                           (avstream-time-base (codec-obj-stream i)))
                       [(or -1 0) min-stream]
                       [1 i])
                     i)))
             (define next-packet
               (if by-index-callback
                   (by-index-callback 'write min-stream)
                   ((dict-ref callback-table (codec-obj-type min-stream) empty-proc) 'write min-stream)))
             (let loop ([next-packet next-packet])
               (cond [(eof-object? next-packet)
                      (set-remove! remaining-streams min-stream)]
                     [(list? next-packet)
                      (map loop next-packet)]
                     [else
                      (match min-stream
                        [(struct* codec-obj ([codec-context codec-context]
                                             [stream stream]))
                         (av-packet-rescale-ts next-packet
                                               (avcodec-context-time-base codec-context)
                                               (avstream-time-base stream))
                         (set-avpacket-stream-index!
                          next-packet (avstream-index (codec-obj-stream min-stream)))
                         (av-write-frame output-context next-packet)
                         ;(av-interleaved-write-frame output-context next-packet)
                         (av-packet-free next-packet)])]))
               #t]))

    (define/public (write-trailer)
      ;(av-interleaved-write-frame output-context #f) ;; <- Maybe not needed?
      (av-write-trailer output-context))
    
    (define/public (close)
      (for ([i (in-vector streams)])
        (match i
          [(struct* codec-obj ([type type]
                               [flags flags]))
           (if by-index-callback
               (by-index-callback 'close i)
               ((dict-ref callback-table type empty-proc) 'close i))]))
      (unless (set-member? (av-output-format-flags format) 'nofile)
        (avio-close (avformat-context-pb output-context)))
      (avformat-free-context output-context))))
  
;; Callback ops:
;;   'init
;;   'open
;;   'write
;;   'close
(define (mux-stream bundle
                    #:callback-table [callback-table (hash)]
                    #:by-index-callback [by-index-callback #f])
  (define mux (new mux%
                   [bundle bundle]
                   [callback-table callback-table]
                   [by-index-callback by-index-callback]))
  (send mux init)
  (send mux open)
  (send mux write-header)
  (let loop ()
    (when (send mux write-packet)
      (loop)))
  (send mux write-trailer)
  (displayln "got here?")
  (send mux close))

;; ===================================================================================================

(struct queue-callback-data (queue
                             codec-obj
                             callback-data)
  #:mutable)
(define (mk-queue-callback-data #:queue q
                                #:codec-obj co
                                #:callback-data cd)
        (queue-callback-data q co cd))

(define ((queue-stream #:passthrough-proc [passthrough-proc #f]) mode obj packet)
  (match obj
    [(struct* codec-obj ([callback-data callback-data]
                         [codec-parameters codec-parameters]
                         [flags flags]))
     (match mode
       ['init (when passthrough-proc
                (passthrough-proc mode obj packet))
              (set-codec-obj-callback-data!
               obj (mk-queue-callback-data #:queue (mk-packetqueue)
                                           #:codec-obj obj
                                           #:callback-data (codec-obj-callback-data obj)))]
       ['loop (define packet* (if passthrough-proc
                                  (passthrough-proc mode obj packet)
                                  packet))
              (packetqueue-put (queue-callback-data-queue callback-data) packet*)]
       ['close (when passthrough-proc
                 (passthrough-proc mode obj packet))
               (packetqueue-put (queue-callback-data-queue callback-data) eof)
               (set-codec-obj-flags!
                obj (set-add flags 'no-close-context))])]))

(define ((dequeue-stream #:passthrough-proc [passthrough-proc #f]) mode obj)
  (match obj
    [(struct* codec-obj ([callback-data callback-data]))
     (match callback-data
       [(struct* queue-callback-data ([codec-obj old-obj]
                                      [queue queue]))
        (define old-context (codec-obj-codec-context old-obj))
        (match mode
          ['init (when passthrough-proc
                   (passthrough-proc mode obj #f old-context))]
          ['open (when passthrough-proc
                   (passthrough-proc mode obj #f old-context))]
          ['write
           (with-handlers ([exn:ffmpeg:again? (λ (e) '())])
             (define data (packetqueue-get queue))
             (if passthrough-proc
                 (passthrough-proc mode obj data old-context)
                 data))]
          ['close (when passthrough-proc
                    (passthrough-proc mode obj #f old-context))
                  (define ctx (codec-obj-codec-context (queue-callback-data-codec-obj callback-data)))
                  (avcodec-close ctx)])])]))

(define (queue-link in-bundle-maker
                    out-bundle-maker
                    #:in-callback [in-callback #f]
                    #:out-callback [out-callback #f])
  (define in-bundle (in-bundle-maker))
  (define in-thread
    ;(thread (λ ()
    (demux-stream in-bundle #:by-index-callback (queue-stream #:passthrough-proc in-callback)));))
  (define out-bundle (out-bundle-maker in-bundle))
  (define out-thread
    ;(thread (λ ()
    (mux-stream out-bundle #:by-index-callback (dequeue-stream #:passthrough-proc out-callback)));))
  (void)
  ;(thread-wait in-thread)
  #;(thread-wait out-thread))

;; ===================================================================================================

(define (mk-render-graph)
  (weighted-graph/directed '()))

(define (racket->ffmpeg x)
  (cond
    [(string? x) x]
    [(integer? x) x]
    [(number? x) (exact->inexact x)]
    [else x]))

(struct node (props counts)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ _ 'node)
      (λ (x) (list '#:counts (node-counts x)))))])
(struct source-node node (bundle)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ _ 'source-node)
      (λ (x) (list '#:file (stream-bundle-file (source-node-bundle x))
                   '#:props (node-props x)
                   '#:counts (node-counts x)))))])
(define (mk-source-node b
                        #:counts [c (hash)]
                        #:props [np (hash)])
  (source-node np c b))
(struct sink-node node (bundle)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ _ 'sink-node)
      (λ (x) (list '#:file (stream-bundle-file (sink-node-bundle x))
                   '#:props (node-props x)
                   '#:counts (node-counts x)))))])
(define (mk-sink-node b
                      #:counts [c (hash)]
                      #:props [np (hash)])
  (sink-node np c b))
(struct filter-node node (table)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ _ 'filter-node)
      (λ (x) (list '#:filters (filter-node-table x)
                   '#:props (node-props x)
                   '#:counts (node-counts x)))))])
(define (mk-filter-node table
                        #:counts [c (hash)]
                        #:props [props (hash)])
  (filter-node props c table))
(struct mux-node node (out-type out-index))
(define (mk-mux-node out-type out-index
                     #:counts [c (hash)]
                     #:props [props (hash)])
  (mux-node props c out-type out-index))
(struct demux-node node (in-counts))
(define (mk-demux-node in-counts
                       #:counts [c (hash)]
                       #:props [props (hash)])
  (demux-node props c in-counts))

(define (mk-empty-video-filter #:width [width DEFAULT-WIDTH]
                               #:height [height DEFAULT-HEIGHT]
                               #:duration [duration 100])
  (mk-filter "color" (let* ([ret (hash "color" "black"
                                       "size" (format "~ax~a"
                                                      (or width DEFAULT-WIDTH)
                                                      (or height DEFAULT-HEIGHT)))]
                            [ret (if duration
                                     (hash-set ret "duration" duration)
                                     ret)])
                       ret)))
(define (mk-empty-audio-filter #:duration [d #f])
  (mk-filter "aevalsrc" (let* ([ret (hash "exprs" "0")]
                               [ret (if d (dict-set ret "d" d) ret)])
                          ret)))
(define (mk-empty-node #:width [width DEFAULT-WIDTH]
                       #:height [height DEFAULT-HEIGHT]
                       #:duration [duration 100]
                       #:counts [counts (hash)]
                       #:props [props (hash)])
  (mk-filter-node (hash 'video (mk-empty-video-filter #:width width
                                                      #:height height
                                                      #:duration duration)
                        'audio (mk-empty-audio-filter))
                  #:counts counts
                  #:props props))
(define (mk-empty-sink-video-filter)
  (mk-filter "nullsink"))
(define (mk-empty-sink-audio-filter)
  (mk-filter "anullsink"))
(define (mk-empty-sink-node #:counts [counts (hash)]
                            #:props (props (hash)))
  (mk-filter-node (hash 'video (mk-empty-sink-video-filter)
                        'audio (mk-empty-sink-audio-filter))
                  #:counts counts
                  #:props props))
(define (mk-fifo-video-filter)
  (mk-filter "fifo"))
(define (mk-fifo-audio-filter)
  (mk-filter "afifo"))
(define (mk-fifo-node #:counts [counts (hash)]
                      #:props [props (hash)])
  (mk-filter-node (hash 'video (mk-fifo-video-filter)
                        'audio (mk-fifo-audio-filter))
                  #:counts counts
                  #:props props))

(define (mk-split-video-filter #:fan-out [c 2])
  (mk-filter "split" (hash 0 c)))
(define (mk-split-audio-filter #:fan-out [c 2])
  (mk-filter "asplit" (hash 0 c)))
(define (mk-split-node #:fan-out [c 2]
                       #:counts [counts (hash)]
                       #:props [props (hash)])
  (mk-filter-node (hash 'video (mk-split-video-filter #:fan-out c)
                        'audio (mk-split-audio-filter #:fan-out c))
                  #:counts counts
                  #:props props))
(define (mk-trim-video-filter #:start start
                              #:end end)
  (mk-filter "trim" (hash "start" (racket->ffmpeg start)
                          "end" (racket->ffmpeg end))))
(define (mk-trim-audio-filter #:start start
                              #:end end)
  (mk-filter "atrim" (hash "start" (racket->ffmpeg start)
                           "end" (racket->ffmpeg end))))
(define (mk-trim-node #:start start
                      #:end end
                      #:counts [counts (hash)]
                      #:props [props (hash)])
  (mk-filter-node (hash 'video (mk-trim-video-filter #:start start
                                                     #:end end)
                        'audio (mk-trim-audio-filter #:start start
                                                     #:end end))
                  #:counts counts
                  #:props props))

(define (mk-reset-timestamp-video-filter [offset #f])
  (mk-filter "setpts" (hash "expr" (format "PTS-STARTPTS~a"
                                           (if offset
                                               (format "+(~a/TB)" offset)
                                               "")))))
(define (mk-reset-timestamp-audio-filter [offset #f])
  (mk-filter "asetpts" (hash "expr" (format "PTS-STARTPTS~a"
                                            (if offset
                                                (format "+(~a/TB)" offset)
                                                "")))))
(define (mk-reset-timestamp-node [offset #f]
                                 #:counts [counts (hash)]
                                 #:props [props (hash)])
  (mk-filter-node (hash 'video (mk-reset-timestamp-video-filter offset)
                        'audio (mk-reset-timestamp-audio-filter offset))
                  #:props props
                  #:counts counts))

(define (color->string color)
  (define c*
    (match color
      [`(,r ,g ,b) (make-object color% r g b)]
      [_ (make-object color% color)]))
  (format "0x~a~a~a~a"
          (number->2string (send c* red))
          (number->2string (send c* green))
          (number->2string (send c* blue))
          (number->2string (inexact->exact (round (* 255 (send c* alpha)))))))

;; Converts a number to a 2 character octal string
;; Number -> String
;; Given: 2 Expect: "02"
;; Given: 255 Expect: "ff"
(define (number->2string number)
  (~a #:left-pad-string "0"
      #:min-width 2
      #:max-width 2
      #:align 'right
      (format "~x" number)))

;; ===================================================================================================

(define (mk-edge-counter [start 0])
  (define edge-counter start)
  (define edge-mapping (make-hash))
  (define (edge-mapping-ref! n vert type i)
    (dict-ref! edge-mapping (vector n vert type i)
               (λ ()
                 (begin0 (format "~a~a" type edge-counter)
                         (set! edge-counter (add1 edge-counter))))))
  (values edge-mapping edge-mapping-ref!))
(define current-edge-mapping-ref! (make-parameter #f))
(define current-edge-mapping (make-parameter #f))
(define current-graph (make-parameter #f))
(define current-graph* (make-parameter #f))

;; For debug purposes only. Print out the edges for sources and sinks.
;; This makes it much easier to use the ffmpeg command line for debugging.
(define debug/current-source-set (make-parameter (mutable-set)))
(define debug/current-sink-set (make-parameter (mutable-set)))

(define (demux-node->string vert)
  (error "TODO"))

;; Mux-Node -> (Listof String)
(define (mux-node->string vert)
  (define out-type (mux-node-out-type vert))
  (define out-index (mux-node-out-index vert))
  (define counts (node-counts vert))
  (define out-str
    (string-append*
     (for/list ([n (get-sorted-neighbors (current-graph) vert)])
       (format "[~a]" ((current-edge-mapping-ref!) vert n out-type 0)))))
  (define in-str
    (append*
     (for/list ([n (get-sorted-neighbors (current-graph*) vert)])
       (for/list ([i (in-range (dict-ref counts vert 0))])
         (format "[~a]" ((current-edge-mapping-ref!) n vert out-type i))))))
  ;; Otput main node and nodes for other types to null
  (append*
   (format "~a~a~a"
           in-str
           (filter->string
            (mk-filter (match out-type
                         ['video "streamselect"]
                         ['audio "astreamselect"])
                       (hash "inputs" (dict-ref counts vert)
                             "map" out-index)))
           out-str)
   (append*
    (for/list ([n (get-sorted-neighbors (current-graph*) vert)])
      (append*
       (for/list ([(type count) (in-dict counts)]
                  #:unless (eq? type out-type))
         (for/list ([i (in-range count)])
           (format "~a~a"
                   ((current-edge-mapping-ref!) n vert type i)
                   (filter->string
                    (match type
                      ['video (mk-empty-sink-video-filter)]
                      ['audio (mk-empty-sink-audio-filter)]))))))))))

;; Filter-Node -> (Listof String)
(define (filter-node->string vert)
  (define node-list
    (append*
     (for/list ([(type count) (in-dict (node-counts vert))])
       (for/list ([i (in-range count)])
         (define out-str
           (string-append*
            (for/list ([n (get-sorted-neighbors (current-graph) vert)])
              (format "[~a]"
                      ((current-edge-mapping-ref!) vert n type i)))))
         (define in-str
           (string-append*
            (for/list ([n (get-sorted-neighbors (current-graph*) vert)])
              (format "[~a]"
                      ((current-edge-mapping-ref!) n vert type i)))))
         (format "~a~a~a"
                 in-str
                 (filter->string
                  (dict-ref (filter-node-table vert) type
                            (λ ()
                              (match type
                                ['video (mk-filter "copy")]
                                ['audio (mk-filter "anull")]))))
                 out-str)))))
  node-list)

;; Source-Node -> (Listof String)
(define (source-node->string vert)
  (append*
   (for/list ([(type count) (in-dict (node-counts vert))])
     (for/list ([i (in-range count)])
       (define in-str
         (let ()
           (define name ((current-edge-mapping-ref!) #f vert type i))
           (define table
             (stream-bundle-stream-table (source-node-bundle vert)))
           (set-codec-obj-callback-data! (dict-ref table type) name)
           (format "[~a]" name)))
       (define out-str
         (string-append*
          (for/list ([n (get-sorted-neighbors (current-graph) vert)])
            (format "[~a]" ((current-edge-mapping-ref!) vert n type i)))))
       (format "~a~a~a"
               in-str
               (match type
                 ['video "fifo"]
                 ['audio "afifo"])
               out-str)))))

;; Because `in-neighbors` returns neighbors in
;; an unspecified order, we need them sorted based
;; on weight
(define (get-sorted-neighbors graph vert)
  (define neighbors (get-neighbors graph vert))
  (sort neighbors
        <
        #:key (λ (x) (edge-weight graph vert x))))

(define filter-types
  '(video audio)) ; subtitle, attachment, and data not supported by ffmpeg

;; Graph -> String
(define (filter-graph->string g)
  (define g* (transpose g))
  (define-values (edge-mapping edge-mapping-ref!) (mk-edge-counter))
  (parameterize ([current-edge-mapping-ref! edge-mapping-ref!]
                 [current-edge-mapping edge-mapping]
                 [current-graph g]
                 [current-graph* g*])
    ;; Graph Source Nodes
    (define src-nodes (base:filter source-node? (get-vertices g)))
    (define bundle-lst (map source-node-bundle src-nodes))
    (define sink-node (findf sink-node? (get-vertices g)))
    (define sink-bundle (sink-node-bundle sink-node))
    ;; Build Graph for each individual codec-obj
    (define node-str-list
      (append*
       (for/list ([vert (in-vertices g)])
         (cond
           [(filter-node? vert)
            (filter-node->string vert)]
           [(mux-node? vert)
            (mux-node->string vert)]
           [(source-node? vert)
            (source-node->string vert)]
           [(sink-node? vert)
            (for ([(type count) (in-dict (node-counts vert))])
              (for ([i (in-range count)])
                (for ([n (get-sorted-neighbors g* vert)])
                  (define table
                    (stream-bundle-stream-table (sink-node-bundle vert)))
                  (set-codec-obj-callback-data! (dict-ref table type)
                                                ((current-edge-mapping-ref!) n vert type i)))))
            '()]
           [else '()]))))
    ;; Different nodes have different stream counts.
    ;; Need to make empty (null) nodes for mismatching
    ;; connections.
    (define cap-nodes-list
      (append*
       (for/list ([(edge name) (in-dict (current-edge-mapping))])
         (match edge
           [(vector src dst type i)
            #:when (and src dst)
            (define src-count (dict-ref (node-counts src) type 0))
            (define dst-count (dict-ref (node-counts dst) type 0))
            (append
             (if (<= src-count i)
                 (list (format
                        "~a[~a]"
                        (filter->string
                         (match type
                           ['video (mk-empty-video-filter)]
                           ['audio (mk-empty-audio-filter)]))
                        name))
                 '())
             (if (<= dst-count i)
                 (list (format
                        "[~a]~a"
                        name
                        (filter->string
                         (match type
                           ['video (mk-empty-sink-video-filter)]
                           ['audio (mk-empty-sink-audio-filter)]))))
                 '()))]
           [_ '()]))))
    (values
     (string-join (append node-str-list cap-nodes-list) ";")
     bundle-lst
     sink-bundle)))

(define (init-filter-graph g)
  (define (make-inout type name [next #f] [args #f])
    (define inout (avfilter-inout-alloc))
    (define inout-ctx (avfilter-graph-create-filter type name args #f graph))
    (set-avfilter-in-out-name! inout (av-strdup name))
    (set-avfilter-in-out-filter-ctx! inout inout-ctx)
    (set-avfilter-in-out-pad-idx! inout 0)
    (set-avfilter-in-out-next! inout next)
    inout)
  (define buffersrc (avfilter-get-by-name "buffer"))
  (define buffersink (avfilter-get-by-name "buffersink"))
  (define abuffersrc (avfilter-get-by-name "abuffer"))
  (define abuffersink (avfilter-get-by-name "abuffersink"))
  (define-values (g-str bundles out-bundle) (filter-graph->string g))
  ;(displayln (graphviz g))
  ;(displayln g-str)
  (define seek-points (get-seek-point g 0))
  (define graph (avfilter-graph-alloc))
  (define outputs
    (for/fold ([ins '()])
              ([bundle bundles])
      (for/fold ([ins ins])
                ([str (stream-bundle-streams bundle)])
        (match str
          [(struct* codec-obj ([type type]
                               [callback-data name]
                               [codec-context ctx]
                               [stream stream]))
           (define type* (match type
                           ['video buffersrc]
                           ['audio abuffersrc]))
           (define args
             (match type
               ['video (format "video_size=~ax~a:pix_fmt=~a:time_base=~a:";pixel_aspect=~a"
                               (avcodec-context-width ctx)
                               (avcodec-context-height ctx)
                               (cast (avcodec-context-pix-fmt ctx) _avpixel-format _int)
                               (avstream-time-base stream)
                               #;(avcodec-context-sample-aspect-ratio ctx))]
               ['audio (format "channel_layout=~a:sample_fmt=~a:time_base=~a:sample_rate=~a"
                               (cast (avcodec-context-channel-layout ctx) _av-channel-layout _uint64)
                               (cast (avcodec-context-sample-fmt ctx) _avsample-format _int)
                               (avcodec-context-time-base ctx)
                               (avcodec-context-sample-rate ctx))]))
           (define n (make-inout type* name (if (null? ins) #f (car ins)) args))
           (set-codec-obj-buffer-context! str (avfilter-in-out-filter-ctx n))
           (set-codec-obj-start-offset! str (dict-ref seek-points bundle))
           (cons n ins)]))))
  (define inputs
    (for/fold ([outs '()])
              ([str (stream-bundle-streams out-bundle)])
      (match str
        [(struct* codec-obj ([type type]
                             [callback-data name]))
         (define type* (match type
                         ['video buffersink]
                         ['audio abuffersink]))
         (define n (make-inout type* name (if (null? outs) #f (car outs))))
         (set-codec-obj-buffer-context! str (avfilter-in-out-filter-ctx n))
         (cons n outs)])))
  ;(avfilter-graph-parse graph g-str (car inputs) (car outputs) #f)
  (define-values (in-ret out-ret)
    (avfilter-graph-parse-ptr graph
                              g-str
                              (if (null? inputs) #f (car inputs))
                              (if (null? outputs) #f (car outputs))
                              #f))
  (avfilter-graph-config graph #f)
  (for ([str (stream-bundle-streams out-bundle)])
    (match str
      [(struct* codec-obj ([codec-context ctx]
                           [buffer-context buff-ctx]
                           [type type]))
       (set-avcodec-context-time-base! ctx (av-buffersink-get-time-base buff-ctx))
       ;(set-avcodec-context-frame-rate! ctx (av-buffersink-get-frame-rate buff-ctx))
       (match type
         ['video (set-avcodec-context-pix-fmt! ctx (av-buffersink-get-format buff-ctx 'video))
                 (set-avcodec-context-width! ctx (av-buffersink-get-w buff-ctx))
                 (set-avcodec-context-height! ctx (av-buffersink-get-h buff-ctx))
                 (set-avcodec-context-sample-aspect-ratio!
                  ctx (av-buffersink-get-sample-aspect-ratio buff-ctx))]
         ['audio (set-avcodec-context-sample-fmt! ctx (av-buffersink-get-format buff-ctx 'audio))
                 (set-avcodec-context-channels! ctx (av-buffersink-get-channels buff-ctx))
                 (set-avcodec-context-channel-layout! ctx (av-buffersink-get-channel-layout buff-ctx))
                 (set-avcodec-context-sample-rate! ctx (av-buffersink-get-sample-rate buff-ctx))])]))
  (avfilter-inout-free in-ret)
  (avfilter-inout-free out-ret)
  (values graph bundles out-bundle))

;; ===================================================================================================

(define ((filtergraph-insert-packet) mode obj packet)
  (match obj
    [(struct* codec-obj ([codec-context ctx]
                         [buffer-context buff-ctx]))
     (match mode
       ['loop
        (avcodec-send-packet ctx packet)
        (with-handlers ([exn:ffmpeg:again? (λ (e) (void))])
          (let loop ()
            (define in-frame (avcodec-receive-frame ctx))
            (set-av-frame-pts!
             in-frame (av-frame-get-best-effort-timestamp in-frame))
            (av-buffersrc-write-frame buff-ctx in-frame)
            ;(av-buffersrc-add-frame buff-ctx in-frame)
            ;(av-buffersrc-add-frame-flags buff-ctx in-frame '(push))
            (av-frame-free in-frame)
            (loop)))
        ]
       ['close
        (avcodec-send-packet ctx #f)
        (with-handlers ([exn:ffmpeg:eof? (λ (e) (void))])
          (let loop ()
            (define in-frame (avcodec-receive-frame ctx))
            (set-av-frame-pts!
             in-frame (av-frame-get-best-effort-timestamp in-frame))
            (av-buffersrc-write-frame buff-ctx in-frame)
            ;(av-buffersrc-add-frame buff-ctx in-frame)
            ;(av-buffersrc-add-frame-flags buff-ctx in-frame '(push))
            (av-frame-free in-frame)
            (loop)))
        (avcodec-flush-buffers ctx)
        (av-buffersrc-write-frame buff-ctx #f)
        ;(av-buffersrc-add-frame buff-ctx #f)
        ;(av-buffersrc-add-frame-flags buff-ctx #f '(push))
        ]
       [_ (void)])]))

(define (filtergraph-next-packet #:render-status [rs-box #f])
  (λ (mode obj)
    (match obj
      [(struct* codec-obj ([codec-context ctx]
                           [buffer-context buff-ctx]))
       (match mode
         ['init
          (when rs-box
            (set-render-status-box-position! rs-box 0)
            (set-render-status-box-rendering?! rs-box #t))]
         ['open
          (av-buffersink-set-frame-size buff-ctx (avcodec-context-frame-size ctx))]
         ['write
          (let loop ()
            (with-handlers ([exn:ffmpeg:again?
                             (λ (e) '())]
                            [exn:ffmpeg:eof?
                             (λ (e)
                               (avcodec-send-frame ctx #f)
                               (let loop ([pkts '()])
                                 (with-handlers ([exn:ffmpeg:eof?
                                                  (λ (e)
                                                    (avcodec-flush-buffers ctx)
                                                    (reverse (cons eof pkts)))])
                                   (define pkt (avcodec-receive-packet ctx))
                                   (loop (cons pkt pkts)))))])
              (define out-frame (av-buffersink-get-frame buff-ctx))
              ;(define out-frame (av-buffersink-get-frame-flags buff-ctx '(no-request)))
              (avcodec-send-frame ctx out-frame)
              (av-frame-free out-frame)
              (let loop ([pkts '()])
                (with-handlers ([exn:ffmpeg:again? (λ (e)
                                                     (when (and rs-box (pair? pkts))
                                                       (set-render-status-box-position!
                                                        rs-box (* (avpacket-pts (car pkts))
                                                                  (avcodec-context-time-base ctx))))
                                                     (reverse pkts))])
                  (define pkt (avcodec-receive-packet ctx))
                  (loop (cons pkt pkts))))))]
         ['close
          (when rs-box
            (set-render-status-box-rendering?! rs-box 'eof))])])))

;; A useful simple rendering function. Only use when debugging, the
;;  video/render module has a much better interface.
(define (render g)
  (define-values (graph in-bundles out-bundle) (init-filter-graph g))
  (define in-threads
    (for/list ([bundle in-bundles]
               [index (in-naturals)])
      (thread
       (λ ()
         (demux-stream
          bundle
          #:by-index-calleback (filtergraph-insert-packet))))))
  (define out-thread
    (thread
     (λ ()
       (mux-stream
        out-bundle
        #:by-index-callback (filtergraph-next-packet)))))
  
  (map thread-wait in-threads)
  (thread-wait out-thread)
  (avfilter-graph-free graph))

;; A proc to render the filtergraph from the command line.
;;   This is useful for diagnosing if a problem is caused by
;;   the encoder/decoder, or the filtergraph itself. Similar interface
;;   to the `render` function above. The `video/render` module is strongly
;;   prefered.
;; WARNING!!!: This function will only work if fmpeg is installed and in the
;;   user's path. This function must ONLY be used for internal
;;   Video development.
(define (render/cmdline-ffmpeg g)
  (define-values (graph in-bundles out-bundle) (filter-graph->string g))
  (define pre-str-list '())
  (define cmd
    (append
     (list ffmpeg)
     (append*
      (for/list ([b in-bundles]
                 [index (in-naturals)])
        (match b
          [(struct* stream-bundle ([stream-table stream-table]
                                   [file file]))
           (set! pre-str-list (cons
                               (format "[~a:v]copy[~a];[~a:a]anull[~a];"
                                       index
                                       (codec-obj-callback-data (dict-ref stream-table 'video))
                                       index
                                       (codec-obj-callback-data (dict-ref stream-table 'audio)))
                               pre-str-list))
           (list "-i" file)])))
     (list
      "-filter_complex"
      (string-append (string-join pre-str-list) graph)
      "-map"
      (format "[~a]" (codec-obj-callback-data (dict-ref (stream-bundle-stream-table out-bundle)
                                                        
                                                        'video)))
      "-map"
      (format "[~a]" (codec-obj-callback-data (dict-ref (stream-bundle-stream-table out-bundle)
                                                        'audio)))
      (stream-bundle-file out-bundle))))
  (displayln cmd)
  (apply system* cmd))

;; ===================================================================================================

;; Given a rendergraph, and an output seek point, find the location
;;    each source node must seek to to result in that seek location
;; Render-Graph Time-In-Seconds -> (Dictof Source-Node Time-In-Seconds)
(define (get-seek-point graph seek-point)
  (define g (graph-copy graph))
  (define g* (transpose g))
  (define-edge-property g* OFFSET 
    #:for-each (OFFSET-set! $from $to
                            (max 0 (- (dict-ref (node-props $from) "start" 0)
                                      (dict-ref (node-props $to) "start" 0)))))
  (define sink-node
    (for/fold ([sink #f])
              ([n (in-vertices g)])
      #:break sink
      (and (sink-node? n) n)))
  (define-vertex-property g* DIST)
  (do-bfs g* sink-node
          #:visit: (DIST-set!
                    $v
                    (if (sink-node? $v)
                        seek-point
                        (for/fold ([offset +inf.0])
                                  ([$from (in-neighbors g $v)])
                          (min offset (+ (DIST $from)
                                         (OFFSET $from $v)))))))
  (for/hash ([n (in-vertices g)]
             #:when (source-node? n))
    (values (source-node-bundle n) (DIST n))))
