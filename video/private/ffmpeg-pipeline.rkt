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
         racket/set
         racket/string
         racket/dict
         graph
         "init.rkt"
         "packetqueue.rkt"
         "ffmpeg.rkt"
         "threading.rkt")

(struct stream-bundle (raw-streams
                       cooked-streams
                       stream-table
                       streams-ready-lock
                       avformat-context
                       options-dict
                       file)
  #:mutable)
(define (mk-stream-bundle #:raw-streams [rs #'()]
                          #:streams [s #'()]
                          #:stream-table [st (make-hash)]
                          #:locked? [locked? #t]
                          #:avformat-context [ctx #f]
                          #:options-dict [o #f]
                          #:file [f #f])
  (define streams-ready (mutex-create))
  (register-video-close mutex-destroy streams-ready)
  (when locked?
    (mutex-lock streams-ready))
  (stream-bundle rs s st streams-ready ctx o f))
(define (stream-bundle-post-streams-ready bundle)
  (mutex-unlock (stream-bundle-streams-ready-lock bundle)))
(define (stream-bundle-streams-ready? bundle)
  (mutex-lock (stream-bundle-streams-ready-lock bundle))
  (mutex-unlock (stream-bundle-streams-ready-lock bundle)))
(define (stream-bundle-streams bundle)
  (stream-bundle-streams-ready? bundle)
  (stream-bundle-cooked-streams bundle))
(define (set-stream-bundle-streams! bundle streams)
  (set-stream-bundle-cooked-streams! bundle streams)
  (stream-bundle-post-streams-ready bundle))

(struct codec-obj (codec-parameters
                   type
                   index
                   id
                   codec
                   codec-context
                   stream
                   next-pts
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
                      #:callback-data [cd #f]
                      #:flags [f '()])
  (codec-obj occ t i id codec codec-context s n cd f))

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
  (mk-stream-bundle #:raw-streams raw-strs
                    #:avformat-context avformat))

(define (demux-stream/fill-bundle bundle
                                  #:video-callback [video-callback empty-proc]
                                  #:audio-callback [audio-callback empty-proc]
                                  #:subtitle-callback [subtitle-callback empty-proc]
                                  #:data-callback [data-callback empty-proc]
                                  #:attachment-callback [attachment-callback empty-proc]
                                  #:by-index-callback [by-index-callback #f])
  ;; Open file
  (define avformat (stream-bundle-avformat-context bundle))
  (define raw-strs (stream-bundle-raw-streams bundle))
  ;(av-dump-format avformat 0 testfile 0)
  ;; Init Streams
  (define stream-table (stream-bundle-stream-table))
  (define streams
    (for/vector ([i raw-strs]
                 [i* (in-naturals)])
      (define codec-parameters (avstream-codecpar i))
      (define codec-name (avcodec-parameters-codec-type codec-parameters))
      (define codec-id (avcodec-parameters-codec-id codec-parameters))
      (define codec (avcodec-find-decoder codec-id))
      (define codec-ctx (avcodec-parameters-to-context codec codec-parameters))
      (avcodec-open2 codec-ctx codec #f)
      (define obj (mk-codec-obj #:codec-parameters codec-parameters
                                #:type codec-name
                                #:index i*
                                #:stream i
                                #:id codec-id
                                #:codec codec
                                #:codec-context codec-ctx))
      (when (and (not by-index-callback) (hash-ref stream-table codec-name #f))
        (error 'decoder-stream "Stream type ~a already present" codec-name))
      (hash-set! stream-table codec-name obj)
      (when by-index-callback
        (by-index-callback 'init obj #f))
      obj))
  (unless by-index-callback
    (for ([(k v) (in-hash stream-table)])
      (match k
        ['video (video-callback 'init v #f)]
        ['audio (audio-callback 'init v #f)]
        ['subtitle (subtitle-callback 'init v #f)]
        ['data (data-callback 'init v #f)]
        ['attachment (attachment-callback 'init v #f)])))
  (set-stream-bundle-streams! bundle streams))

(define (demux-stream/close-bundle bundle
                                   #:video-callback [video-callback empty-proc]
                                   #:audio-callback [audio-callback empty-proc]
                                   #:subtitle-callback [subtitle-callback empty-proc]
                                   #:data-callback [data-callback empty-proc]
                                   #:attachment-callback [attachment-callback empty-proc]
                                   #:by-index-callback [by-index-callback #f])
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-streams bundle))
  (define stream-table (stream-bundle-stream-table))
  (unless by-index-callback
    (for ([(k v) (in-hash stream-table)])
      (match k
        ['video (video-callback 'close v #f)]
        ['audio (audio-callback 'close v #f)]
        ['subtitle (subtitle-callback 'close v #f)]
        ['data (data-callback 'close v #f)]
        ['attachment (attachment-callback 'close v #f)])))
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
                      #:video-callback [video-callback empty-proc]
                      #:audio-callback [audio-callback empty-proc]
                      #:subtitle-callback [subtitle-callback empty-proc]
                      #:data-callback [data-callback empty-proc]
                      #:attachment-callback [attachment-callback empty-proc]
                      #:by-index-callback [by-index-callback #f])
  ;; Init Streams
  (define avformat (stream-bundle-avformat-context bundle))
  (define streams (stream-bundle-streams bundle))
  (define stream-table (stream-bundle-stream-table))
  (demux-stream/fill-bundle bundle
                            #:video-callback video-callback
                            #:audio-callback audio-callback
                            #:subtitle-callback subtitle-callback
                            #:data-callback data-callback
                            #:attachment-callback attachment-callback
                            #:by-index-callback by-index-callback)
  ;; Main Loop
  (let loop ()
    (define packet (av-read-frame avformat))
    (when packet
      (define index (avpacket-stream-index packet))
      (define obj (vector-ref streams index))
      (cond [by-index-callback (by-index-callback 'loop obj packet)]
            [else
             (define type (codec-obj-type obj))
             (cond [(eq? obj (hash-ref stream-table type))
                    (match type
                      ['video (video-callback 'loop obj packet)]
                      ['audio (audio-callback 'loop obj packet)]
                      ['subtitle (subtitle-callback 'loop obj packet)]
                      ['data (data-callback 'loop obj packet)]
                      ['attachment (attachment-callback 'loop obj packet)]
                      [_ (av-packet-unref packet)])]
                   [else (av-packet-unref packet)])])
      (loop)))
  ;; Close Down
  (demux-stream/close-bundle bundle
                             #:video-callback video-callback
                             #:audio-callback audio-callback
                             #:subtitle-callback subtitle-callback
                             #:data-callback data-callback
                             #:attachment-callback attachment-callback
                             #:by-index-callback by-index-callback))

(define (bundle-for-file file bundle
                         #:output-format [output-format #f]
                         #:format-name [format-name #f]
                         #:options-dict [options-dict #f])
  (define streams
    (for/vector ([i (stream-bundle-streams bundle)])
      (match i
        [(struct* codec-obj ([type t]
                             [id i]
                             [callback-data callback-data]))
         (mk-codec-obj #:type t
                       #:id i
                       #:callback-data callback-data)]
        [x (mk-codec-obj #:type x)])))
  (define output-context
    (avformat-alloc-output-context2 output-format format-name file))
  (mk-stream-bundle #:avformat-context output-context
                    #:options-dict options-dict
                    #:file file
                    #:streams streams
                    #:locked? #f))

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
(define (mk-source-node #:bundle [b #f]
                        #:props [np (hash)])
  (source-node b np))
(struct filter-node node (table))
(define (mk-filter-node table
                        #:props [props (hash)])
  (filter-node table props))

(define (convert-graph g bundle)
  (define g* (transpose g))
  (define (build-stream-subgraph type prefix)
    (define edge-counter 0)
    (define edge-mapping (make-hash))
    (string-append*
     (for/list ([vert (in-vertices g)])
       (define out-str
         (string-append*
          (for/list ([n (in-neighbors g vert)])
            (format "[~a]"
                    (dict-ref! edge-mapping (cons vert n)
                               (λ ()
                                 (begin0 (format "~a~a" prefix edge-counter)
                                         (set! edge-counter (add1 edge-counter)))))))))
       (define in-str
         (string-append*
          (for/list ([n (in-neighbors g* vert)])
            (format "[~a]"
                    (dict-ref! edge-mapping (cons n vert)
                               (λ ()
                                 (begin0 (format "~a~a" prefix edge-counter)
                                         (set! edge-counter (add1 edge-counter)))))))))
       (format "~a~a~a;"
               in-str
               (dict-ref filter-node-table type
                         (λ ()
                           (match type
                             ['video "copy"]
                             ['audio "acopy"])))
               out-str))))
  (string-append*
   (for/list ([str (stream-bundle-streams bundle)]
              [index (in-naturals)])
     (build-stream-subgraph (codec-obj-type str)
                            (format "str~a~a" index (codec-obj-id str))))))

(define (init-filters g bundle)
  (define (make-inout name type)
    (define inout (avfilter-inout-alloc))
    ...)
  (define buffersrc (avfilter-get-by-name "buffer"))
  (define buffersink (avfilter-get-by-name "buffersink"))
  (define abuffersrc (avfilter-get-by-name "abuffer"))
  (define abuffersink (avfilter-get-by-name "abuffersink"))
  (define outputs (avfilter-inout-alloc))
  (define inputs (avfilter-inout-alloc))
  (define graph (avfilter-graph-alloc))
  (define g-str (convert-graph graph bundle))
  (define buffersrc-ctx (avfilter-graph-create-filter buffersrc "IN-TODO!!!" #f #f graph))
  (define buffersink-ctx (avfilter-graph-create-filter buffersink "OUT-TODO!!!" #f #f graph))
  (void))
