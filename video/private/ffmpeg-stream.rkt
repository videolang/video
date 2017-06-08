#lang racket/base

(provide (all-defined-out))
(require racket/match
         "ffmpeg.rkt")

(struct codec-obj (orig-codec-context
                   type
                   index
                   id
                   codec
                   codec-context)
  #:mutable)

(define (stream-file file
                     #:video-callback [video-callback #f]
                     #:audio-callback [audio-callback #f]
                     #:subtitle-callback [subtitle-callback #f]
                     #:data-callback [data-callback #f]
                     #:attachment-callback [attachment-callback #f]
                     #:by-index-callback [by-index-callback #f])
  (define avformat (avformat-open-input file #f #f))
  (avformat-find-stream-info avformat #f)
  ;(av-dump-format avformat 0 testfile 0)
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
      (define obj (codec-obj old-codec-ctx codec-name i* codec-id codec codec-ctx))
      (hash-set! stream-table codec-name obj)
      obj))
  (let loop ()
    (define packet (av-read-frame avformat))
    (when packet
      (define index (avpacket-stream-index packet))
      (define obj (vector-ref streams index))
      (cond [by-index-callback (by-index-callback obj)]
            [else
             (define type (codec-obj-type obj))
             (cond [(eq? obj (hash-ref stream-table type))
                    (match type
                      ['video (video-callback obj)]
                      ['audio (audio-callback obj)]
                      ['subtitle (subtitle-callback obj)]
                      ['data (data-callback obj)]
                      ['attachment (attachment-callback obj)]
                      [_ (av-packet-unref packet)])]
                   [else (av-packet-unref packet)])])
      (loop)))
  (for ([i (in-vector streams)])
    (match i
      [(struct* codec-obj
                ([orig-codec-context orig-codec-context]
                 [codec-context codec-context]))
       (avcodec-close orig-codec-context)
       (avcodec-close codec-context)]))
  (avformat-close-input avformat))
