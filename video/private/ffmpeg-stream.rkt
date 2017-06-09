#lang racket/base

(provide (all-defined-out))
(require racket/match
         ffi/unsafe
         racket/set
         "init-mlt.rkt"
         "ffmpeg.rkt")

(struct codec-obj (orig-codec-context
                   type
                   index
                   id
                   codec
                   codec-context
                   stream)
  #:mutable)

(define (empty-proc mode obj packet)
  (when packet
    (av-packet-unref packet)))

(define (decoder-stream file
                        #:video-callback [video-callback empty-proc]
                        #:audio-callback [audio-callback empty-proc]
                        #:subtitle-callback [subtitle-callback empty-proc]
                        #:data-callback [data-callback empty-proc]
                        #:attachment-callback [attachment-callback empty-proc]
                        #:by-index-callback [by-index-callback #f])
  ;; Open file
  (define avformat (avformat-open-input file #f #f))
  (avformat-find-stream-info avformat #f)
  ;(av-dump-format avformat 0 testfile 0)
  ;; Init Streams
  (define raw-strs (avformat-context-streams avformat))
  (define stream-table (make-hash))
  (define streams
    (for/vector ([i raw-strs]
                 [i* (in-naturals)])
      (define old-codec-ctx (avstream-codec i))
      (define codec-name (avcodec-context-codec-type* old-codec-ctx))
      (define codec-id (avcodec-context-codec-id old-codec-ctx))
      (define codec (avcodec-find-decoder codec-id))
      (define codec-ctx (avcodec-copy-context codec old-codec-ctx))
      (avcodec-open2 codec-ctx codec #f)
      (define obj (codec-obj old-codec-ctx codec-name i* codec-id codec codec-ctx #f))
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
      [(struct* codec-obj
                ([orig-codec-context orig-codec-context]
                 [codec-context codec-context]
                 [index index]))
       (when by-index-callback
         (by-index-callback 'close i #f))
       (avcodec-close orig-codec-context)
       (avcodec-close codec-context)]))
  (avformat-close-input avformat))

(define (encoder-stream file
                        #:video-callback [video-callback empty-proc]
                        #:audio-callback [audio-callback empty-proc]
                        #:subtitle-callback [subtitle-callback empty-proc]
                        #:data-callback [data-callback empty-proc]
                        #:attachment-callback [attachment-callback empty-proc]
                        #:by-index-callback [by-index-callback #f])
  (define output-context
    (avformat-alloc-output-context2 #f #f file))
  (define format (avformat-context-oformat output-context))
  (define video-codec (av-output-format-video-codec format))
  (define audio-codec (av-output-format-audio-codec format))
  (define subtitle-codec (av-output-format-subtitle-codec format))
  (define stream-table (make-hash))
  (define streams
    (cond [by-index-callback
           (for/vector ([i (in-range (by-index-callback 'count))])
             (by-index-callback 'get i))]
          [else
           (and video-callback (hash-set! stream-table 'video (video-callback 'get)))
           (and audio-callback (hash-set! stream-table 'audio (audio-callback 'get)))
           (and subtitle-callback (hash-set! stream-table 'subtitle (subtitle-callback 'get)))
           (and data-callback (hash-set! stream-table 'data (data-callback 'get)))
           (and attachment-callback (hash-set! stream-table 'attachment (attachment-callback 'get)))
           (for/vector ([(k v) (in-hash stream-table)])
             v)]))
  (for ([i streams])
    (match i
      [(struct* codec-obj
                ([id id]
                 [type type]))
       (define type-codec-id
         (match type
           ['video video-codec]
           ['audio audio-codec]
           ['subtitle subtitle-codec]
           [else #f]))
       (define codec-id (or id type-codec-id))
       (define codec (avcodec-find-encoder codec-id))
       (define str (avformat-new-stream output-context #f))
       (set-avstream-id! str (sub1 (avformat-context-nb-streams output-context)))
       (define ctx (avcodec-alloc-context3 codec))
       (match type
         ['video
          (set-avcodec-context-codec-id! ctx codec-id)
          (set-avcodec-context-bit-rate! ctx 400000)
          (set-avcodec-context-width! ctx 1920)
          (set-avcodec-context-height! ctx 1080)
          (set-avstream-time-base! str 25)
          (set-avcodec-context-time-base! ctx 25)
          (set-avcodec-context-gop-size! ctx 12)
          (set-avcodec-context-pix-fmt! ctx 'yuv420p)
          (when (eq? codec-id 'mpeg2video)
            (set-avcodec-context-max-b-frames! ctx 2))
          (when (eq? codec-id 'mpeg1video)
            (set-avcodec-context-mb-decision! ctx 2))]
         ['audio
          (set-avcodec-context-sample-fmt!
           ctx (if (avcodec-sample-fmts codec)
                   (ptr-ref (avcodec-sample-fmts codec))
                   'fltp))
          (set-avcodec-context-bit-rate! ctx 64000)
          (define supported-samplerates (avcodec-supported-samplerates codec))
          (set-avcodec-context-sample-rate!
           ctx
           (if supported-samplerates
               (let loop ([rate #f]
                          [offset 0])
                 (define new-rate (ptr-ref (avcodec-supported-samplerates codec)
                                           offset))
                 (cond [(= new-rate 44100) new-rate]
                       [(= new-rate 0) (or rate 44100)]
                       [else (loop (or rate new-rate) (add1 offset))]))
               44100))
          (define supported-layouts (avcodec-channel-layouts codec))
          (set-avcodec-context-channel-layout!
           ctx
           (if supported-layouts
               (or
                (let loop ([layout #f]
                           [offset 0])
                  (define new-layout (ptr-ref (avcodec-channel-layouts codec)
                                              offset))
                  (cond [(set-member? new-layout 'stereo) 'stereo]
                        [(set-empty? new-layout) (or layout 'stereo)]
                        [else (loop (or layout new-layout) (add1 offset))])))
               'stereo))
          (set-avcodec-context-channels!
           ctx (av-get-channel-layout-nb-channels (avcodec-context-channel-layout ctx)))
          (set-avstream-time-base! str (/ 1 (avcodec-context-sample-rate ctx)))]
         [else (void)])
       (when (set-member? (avformat-context-flags output-context) 'globalheader)
         (set-add! (avcodec-context-flags ctx) 'global-heade))])))
