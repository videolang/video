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

(provide (all-defined-out))
(require racket/match
         ffi/unsafe
         racket/list
         racket/set
         racket/string
         racket/dict
         (prefix-in base: racket/base)
         graph
         "init.rkt"
         "packetqueue.rkt"
         "ffmpeg.rkt"
         "threading.rkt")

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

(struct codec-obj (codec-parameters
                   type
                   index
                   id
                   codec
                   codec-context
                   stream
                   next-pts
                   buffer-context
                   callback-data
                   flags)
  #:mutable)
(define (mk-codec-obj #:codec-parameters [occ #f]
                      #:type [t #f]
                      #:index [i #f]
                      #:id [id #f]
                      #:codec [codec #f]
                      #:codec-context [codec-context #f]
                      #:stream [s #f]
                      #:next-pts [n 0]
                      #:buffer-context [bc #f]
                      #:callback-data [cd #f]
                      #:flags [f '()])
  (codec-obj occ t i id codec codec-context s n bc cd f))

(struct filter (name
                args
                instance-name))
(define (mk-filter name
                   #:args [args (hash)]
                   #:instance-name [instance-name #f])
  (filter name args instance-name))
(define (filter->string filter)
  (string-append (filter-name filter)
                 (if (null? (filter-args filter)) "" "=")
                 (string-join
                  (for/list ([(k v) (in-dict filter-args filter)])
                    (format "~a=~a" k v))
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
                    #:streams streams))

(define (demux-stream/fill-bundle bundle
                                  #:callback-table [callback-table (hash)]
                                  #:by-index-callback [by-index-callback #f])
  ;; Open file
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-raw-streams bundle))
  ;(av-dump-format avformat 0 testfile 0)
  ;; Init Streams
  (define stream-table (stream-bundle-stream-table))
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
      ((callback-ref callback-table k)) 'init v #f)))

(define (demux-stream/loop bundle
                           #:callback-table [callback-table (hash)]
                           #:by-index-callback [by-index-callback #f])
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-streams bundle))
  (define stream-table (stream-bundle-stream-table))
  (let loop ()
    (define packet (av-read-frame avformat))
    (when packet
      (define index (avpacket-stream-index packet))
      (define obj (vector-ref streams index))
      (cond [by-index-callback (by-index-callback 'loop obj packet)]
            [else
             (define type (codec-obj-type obj))
             (cond [(eq? obj (hash-ref stream-table type))
                    ((callback-ref callback-table type) 'loop obj packet)]
                   [else (av-packet-unref packet)])])
      (loop))))

(define (demux-stream/close-bundle bundle
                                   #:callback-table [callback-table (hash)]
                                   #:by-index-callback [by-index-callback #f])
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-streams bundle))
  (define stream-table (stream-bundle-stream-table))
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
  (avformat-close-input avformat))

;; Callback ops:
;;   'init
;;   'loop
;;   'close
(define (demux-stream bundle
                      #:callback-table [callback-table (hash)]
                      #:by-index-callback [by-index-callback #f])
  ;; Init Streams
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-streams bundle))
  (define stream-table (stream-bundle-stream-table))
  (demux-stream/fill-bundle bundle
                            #:callback-table [callback-table (hash)]
                            #:by-index-callback by-index-callback)
  ;; Main Loop
  (demux-stream/loop bundle
                     #:callback-table [callback-table (hash)]
                     #:by-index-callback by-index-callback)
  ;; Close Down
  (demux-stream/close-bundle bundle
                             #:callback-table [callback-table (hash)]
                             #:by-index-callback by-index-callback))

(define (bundle-for-file file bundle/spec
                         #:output-format [output-format #f]
                         #:format-name [format-name #f]
                         #:options-dict [options-dict #f])
  (define streams
    (match bundle/spec
      [(struct* stream-bundle ([streams streams]))
       (for/vector ([i streams])
         (match i
           [(struct* codec-obj ([type t]
                                [id i]
                                [index index]
                                [callback-data callback-data]))
            (mk-codec-obj #:type t
                          #:id i
                          #:index index
                          #:callback-data callback-data)]
           [x (mk-codec-obj #:type x)]))]
      [(or 'vid+aud 'video+audio 'movie)
       (vector
        (mk-codec-obj #:type 'video
                      #:index 0)
        (mk-codec-obj #:type 'audio
                      #:index 1))]))
  (define output-context
    (avformat-alloc-output-context2 output-format format-name file))
  (mk-stream-bundle #:avformat-context output-context
                    #:options-dict options-dict
                    #:file file
                    #:streams streams))

;; Callback ops:
;;   'init
;;   'open
;;   'write
;;   'close
(define (mux-stream bundle
                    #:video-callback [video-callback empty-proc]
                    #:audio-callback [audio-callback empty-proc]
                    #:subtitle-callback [subtitle-callback empty-proc]
                    #:data-callback [data-callback empty-proc]
                    #:attachment-callback [attachment-callback empty-proc]
                    #:by-index-callback [by-index-callback #f])
  ;; Initial Setup
  (define output-context
    (stream-bundle-avformat-context bundle))
  (define options (convert-dict (stream-bundle-options-dict bundle)))
  (define file (stream-bundle-file bundle))
  (define format (avformat-context-oformat output-context))
  (define video-codec (av-output-format-video-codec format))
  (define audio-codec (av-output-format-audio-codec format))
  (define subtitle-codec (av-output-format-subtitle-codec format))
  ;; Get streams
  (define stream-table (make-hash))
  (define streams (stream-bundle-streams bundle))
  ;; Get codec and other attributes of decoded video
  (for ([i (in-vector streams)]
        [index (in-naturals)])
    (match i
      [(struct* codec-obj ([id id]
                           [type type]))
       (define type-codec-id
         (match type
           ['video video-codec]
           ['audio audio-codec]
           ['subtitle subtitle-codec]
           [else #f]))
       (define codec-id (or id type-codec-id))
       (define codec (avcodec-find-encoder codec-id))
       (set-codec-obj-codec! i codec)
       (define str (avformat-new-stream output-context #f))
       (set-codec-obj-stream! i str)
       (set-codec-obj-id! i codec-id)
       (set-codec-obj-index! i index)
       (set-avstream-id! str (sub1 (avformat-context-nb-streams output-context)))
       (define ctx (avcodec-alloc-context3 codec))
       (set-codec-obj-codec-context! i ctx)
       (if by-index-callback
           (by-index-callback 'init i)
           (match type
             ['video (video-callback 'init i)]
             ['audio (audio-callback 'init i)]
             ['subtitle (subtitle-callback 'init i)]
             ['data (data-callback 'init i)]
             ['attachment (attachment-callback 'init i)]))
       (when (set-member? (avformat-context-flags output-context) 'globalheader)
         (set-avcodec-context-flags!
          ctx (set-add (avcodec-context-flags ctx) 'global-header)))]))
  ;; Open Streams
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
           (match type
             ['video (video-callback 'open i)]
             ['audio (audio-callback 'open i)]
             ['subtitle (subtitle-callback 'open i)]
             ['data (data-callback 'open i)]
             ['attachment (attachment-callback 'open i)]))
       (avcodec-parameters-from-context (avstream-codecpar stream) ctx)]))
  ;; Create file.
  ;(av-dump-format output-context 0 file 'output)
  (unless (set-member? (av-output-format-flags format) 'nofile)
    (set-avformat-context-pb!
     output-context (avio-open (avformat-context-pb output-context) file 'write)))
  ;; Write the stream
  (avformat-write-header output-context #f)
  (define remaining-streams (mutable-set))
  (for ([i (in-vector streams)])
    (set-add! remaining-streams i))
  (let loop ()
    (unless (set-empty? remaining-streams)
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
            (match (codec-obj-type min-stream)
              ['video (video-callback 'write min-stream)]
              ['audio (audio-callback 'write min-stream)]
              ['subtitle (subtitle-callback 'write min-stream)]
              ['data (data-callback 'write min-stream)]
              ['attachment (attachment-callback 'write min-stream)])))
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
                  (av-interleaved-write-frame output-context next-packet)])]))
      (loop)))
  (av-write-trailer output-context)
  ;; Clean Up
  (for ([i (in-vector streams)])
    (match i
      [(struct* codec-obj ([type type]
                           [flags flags]))
       (if by-index-callback
           (by-index-callback 'close i)
           (match type
             ['video (video-callback 'close i)]
             ['audio (audio-callback 'close i)]
             ['subtitle (subtitle-callback 'close i)]
             ['data (data-callback 'close i)]
             ['attachment (attachment-callback 'close i)]))]))
  (unless (set-member? (av-output-format-flags format) 'nofile)
    (avio-close (avformat-context-pb output-context)))
  (avformat-free-context output-context))

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

(struct node (props))
(struct source-node node (bundle))
(define (mk-source-node b
                        #:props [np (hash)])
  (source-node np b))
(struct sink-node node (bundle)
  #:mutable)
(define (mk-sink-node b
                      #:props [np (hash)])
  (sink-node np b))
(struct filter-node node (table))
(define (mk-filter-node table
                        #:props [props (hash)])
  (filter-node props table))

(define (filter-graph->string g)
  (define g* (transpose g))
  ;; Graph Source Nodes
  (define src-nodes (base:filter source-node? (get-vertices g)))
  (define bundle-lst (map source-node-bundle src-nodes))
  (define sink-node (findf sink-node? (get-vertices g)))
  (define sink-bundle (sink-node-bundle sink-node))
  ;; Build Graph
  (define (get-sorted-neighbors graph vert)
    (define neighbors (get-neighbors graph vert))
    (sort neighbors
          <
          #:key (λ (x) (edge-weight graph vert x))))
  (define (build-stream-subgraph type prefix)
    (define edge-counter 0)
    (define edge-mapping (make-hash))
    (string-append*
     (for/list ([vert (in-vertices g)])
       (cond
         [(filter-node? vert)
          (define out-str
            (string-append*
             (for/list ([n (get-sorted-neighbors g vert)])
               (format "[~a]"
                       (dict-ref! edge-mapping (cons vert n)
                                  (λ ()
                                    (let ([name (format "~a~a" prefix edge-counter)])
                                      (when (sink-node? n)
                                        (define table
                                          (stream-bundle-stream-table (sink-node-bundle n)))
                                        (set-codec-obj-callback-data! (dict-ref table type) name))
                                      (set! edge-counter (add1 edge-counter))
                                      name)))))))
          (define in-str
            (string-append*
             (for/list ([n (get-sorted-neighbors g* vert)])
               (format "[~a]"
                       (dict-ref! edge-mapping (cons n vert)
                                  (λ ()
                                    (let ([name (format "~a~a" prefix edge-counter)])
                                      (when (source-node? n)
                                        (define table
                                          (stream-bundle-stream-table (source-node-bundle n)))
                                        (set-codec-obj-callback-data! (dict-ref table type) name))
                                      (set! edge-counter (add1 edge-counter))
                                      name)))))))
          (format "~a~a~a;"
                  in-str
                  (filter->string
                   (dict-ref filter-node-table type
                            (λ ()
                              (match type
                                ['video (mk-filter "copy")]
                                ['audio (mk-filter "acopy")]))))
                  out-str)]
         [else ""]))))
  (values
   (string-append*
    (append* (for/list ([bundle (in-list bundle-lst)])
               (for/list ([str (stream-bundle-streams bundle)]
                          [index (in-naturals)])
                 (build-stream-subgraph (codec-obj-type str)
                                        (format "str~a~a" index (codec-obj-id str)))))))
   bundle-lst
   sink-bundle))

(define (init-filter-graph g)
  (define (make-inout type name [next #f] [args #f])
    (define inout (avfilter-inout-alloc))
    (define inout-ctx (avfilter-graph-create-filter type name args #f graph))
    (set-avfilter-in-out-name! inout name)
    (set-avfilter-in-out-filter-ctx! inout inout-ctx)
    (set-avfilter-in-out-pad-idx! inout 0)
    (set-avfilter-in-out-next! inout next)
    inout)
  (define buffersrc (avfilter-get-by-name "buffer"))
  (define buffersink (avfilter-get-by-name "buffersink"))
  (define abuffersrc (avfilter-get-by-name "abuffer"))
  (define abuffersink (avfilter-get-by-name "abuffersink"))
  (define-values (g-str bundles out-bundle) (filter-graph->string g))
  (define graph (avfilter-graph-alloc))
  (define outputs
    (for/fold ([outs '()])
              ([str (stream-bundle-streams out-bundle)])
      (match str
        [(struct* codec-obj ([type type]
                             [callback-data name]))
         (define type* (match type
                         ['video buffersrc]
                         ['audio abuffersrc]))
         (define n (make-inout type* name (if (null? outs) #f (car outs))))
         (cons n outs)])))
  (define inputs
    (for/fold ([ins '()])
              ([bundle bundles])
      (for/fold ([ins ins])
                ([str (stream-bundle-streams bundle)])
        (match str
          [(struct* codec-obj ([type type]
                               [callback-data name]
                               [codec-context ctx]))
           (define type* (match type
                           ['video buffersink]
                           ['audio abuffersink]))
           (define args
             (match type
               ['video (format "video_size=~ax~a:pix_fmt=~a:time_base=~a:pixel_aspect=~a"
                               (avcodec-context-width ctx)
                               (avcodec-context-height ctx)
                               (cast (avcodec-context-pix-fmt ctx) _avpixel-format _int)
                               (avcodec-context-time-base ctx)
                               (avcodec-context-sample-aspect-ratio ctx))]
               ['audio (format "channel_layout=~a:sample_fmt=~a:time_base=~a:sample_rate=~a"
                               (cast (avcodec-context-channel-layout ctx) _av-channel-layout _int)
                               (cast (avcodec-context-sample-fmt ctx) _avsample-format _int)
                               (avcodec-context-time-base ctx)
                               (avcodec-context-sample-rate ctx))]))
           (define n (make-inout type* name (if (null? ins) #f (car ins)) args))
           (cons n ins)]))))
  (define-values (in-ret out-ret) (avfilter-graph-parse-ptr graph g-str inputs outputs #f))
  (avfilter-inout-free in-ret)
  (avfilter-inout-free out-ret)
  (values graph bundles out-bundle))

(define (render g)
  (define-values (graph in-bundles out-bundle) (init-filter-graph g))
  (for ([bundle in-bundles])
    (demux-stream
     bundle
     #:by-index-callback (λ (mode obj packet)
                           (match obj
                             [(struct* codec-obj ([codec-context ctx]
                                                  [buffer-context buff-ctx]))
                              (match mode
                                ['loop
                                 (avcodec-send-packet ctx packet)
                                 (define frame (avcodec-receive-frame ctx))
                                 (set-av-frame-pts!
                                  frame (av-frame-get-best-effort-timestamp frame))
                                 (av-buffersrc-add-frame buff-ctx frame)])]))
  (mux-stream
   out-bundle
   #:by-index-callback (λ (mode obj)
                         (match obj
                           [(struct* codec-obj ([codec-context ctx]
                                                [buffer-context buff-ctx]))
                            (match mode
                              ['write
                               (let loop ()
                                 (with-handlers ([exn:ffmpeg:again? (λ (e) (loop))]
                                                 [exn:ffmpeg:eof? (λ (e) eof)])
                                   (define frame (av-buffersink-get-frame buff-ctx))
                                   (let loop ()
                                     (with-handlers ([exn:ffmpeg:again? (λ (e) (loop))])
                                       (avcodec-send-frame ctx frame)
                                       (define pkt (avcodec-receive-packet ctx))
                                       pkt))))])])))))
  (avfilter-graph-free graph))
