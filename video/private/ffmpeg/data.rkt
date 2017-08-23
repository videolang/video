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
(require "lib.rkt"
         "constants.rkt"
         ffi/unsafe
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(define-syntax (define-ffmpeg-cstruct stx)
  (syntax-parse stx
    [(_ id ([field-name field-rest ...] ...) rest ...)
     #:with id-field-names (format-id stx "~a-field-names" #'id)
     (quasisyntax/loc stx
       (begin
         #,(quasisyntax/loc stx
             (define id-field-names '#,#'(field-name ...)))
         #,(syntax/loc stx
             (define-cstruct id ([field-name field-rest ...] ...) rest ...))))]))


(define _avrational
  (let ()
    (define-cstruct _avrational
      ([num _int]
       [den _int]))
    (make-ctype _avrational
                (λ (x)
                  (make-avrational (numerator x)
                                   (denominator x)))
                (λ (x)
                  (when (= (avrational-den x) 0)
                    (error 'avrational "Invalid AVRational ~a/~a"
                           (avrational-num x)
                           (avrational-den x)))
                  (/ (avrational-num x)
                     (avrational-den x))))))

(define-cstruct _av-dictionary-entry
  ([key _pointer]
   [value _pointer]))

(define-cstruct _av-dictionary
  ([count _int]
   [elems _av-dictionary-entry-pointer/null]))

(define-cstruct _avclass
  ([class-name _string]
   [item-name _fpointer]
   [option _pointer]
   [version _int]
   [log-level-offset-offset _int]
   [parent-log-context-offset _int]
   [child-next _pointer]
   [catagory _avclass-category]
   [get-catagory _fpointer]
   [query-range _fpointer]))

(define-cstruct _avio-context
  ([buffer _bytes]
   [buffer-size _int]
   [buf-ptr _bytes]
   [buf-end _bytes]
   [opeque _pointer]
   [read-packet _fpointer]
   [write-packet _fpointer]
   [seek _fpointer]
   [pos _int64]
   [must-flush _int]
   [eof-reached _int]
   [write-flag _int]
   [max-packet-size _int]
   [checksum _ulong]
   [checksum-ptr _pointer]
   [update-checksum _fpointer]
   [error _int]
   [read-pause _fpointer]
   [read-seek _fpointer]
   [seekable _int]
   [maxsize _int64]
   [direct _int]
   [bytes-read _int64]
   [seek-count _int]
   [writeout-count _int]
   [orig-buffer-size _int]))

(define-cstruct _avio-interrupt-cb
  ([callback _fpointer]
   [opaque _pointer]))

(define-cstruct _av-input-format
  ([name _string]
   [long-name _string]
   [flags _avformat-flags]
   [extensions _string]
   [codec-tag _pointer]
   [priv-class _pointer]
   [mime-type _string]
   [next _pointer]
   [raw-codec-id _int]
   [priv-data-size _int]
   [read-probe _fpointer]
   [read-header _fpointer]
   [read-packet _fpointer]
   [read-close _fpointer]
   [read-seek _fpointer]
   [read-timestamp _fpointer]
   [read-play _fpointer]
   [read-pause _fpointer]
   [read-seek2 _fpointer]
   [get-device-list _fpointer]
   [create-device-capabilities _fpointer]
   [free-device-capabilities _fpointer]))

(define-cstruct _av-output-format
  ([name _string]
   [long-name _string]
   [mime-type _string]
   [extensions _string]
   [audio-codec _avcodec-id]
   [video-codec _avcodec-id]
   [subtitle-codec _avcodec-id]
   [flags _avformat-flags]
   [codec-tag _pointer]
   [priv-class _pointer]))

(define-ffmpeg-cstruct _avchapter
  ([id _int]
   [time-base _avrational]
   [start _int64]
   [end _int64]
   [metadata _av-dictionary-pointer/null]))
(define (mk-avchapter #:id [id 0]
                      #:time-base [tb 0]
                      #:start [s 0]
                      #:end [e 0]
                      #:metadata [m #f])
  (make-avchapter id tb s e m))

(define-ffmpeg-cstruct _avformat-context
  ([av-class _pointer]
   [iformat _av-input-format-pointer/null]
   [oformat _av-output-format-pointer/null]
   [priv_data _pointer]
   [pb _avio-context-pointer/null]
   [ctx-flags _int] ; _avformat-context-flags
   [nb-streams _uint]
   [streams-data _pointer]
   [filename (_array _byte 1024)]
   [start-time _int64]
   [duration _int64]
   [bit-rate _int]
   [packet-size _uint]
   [max-delay _uint]
   [flags _avformat-flags]
   [probesize _uint]
   [max-anlayze-duration _int]
   [key _pointer]
   [keylen _int]
   [nb-programs _uint]
   [programs _pointer]
   [video-codec-id _avcodec-id]
   [audio-codec-id _avcodec-id]
   [subtitle-codec-id _avcodec-id]
   [max-index-size _uint]
   [max-picture-buffer _uint]
   [nb-chapters _uint]
   [chapters-data _pointer]
   [metadata _pointer]
   [start-time-realtime _int64]
   [fps-probe-size _int]
   [error-recognition _int]
   [interrupt-callback _avio-interrupt-cb]
   [debug _int]
   [max-interleave-delay _int64]
   [string-std-compliance _int]
   [event-flags _int]
   [max-ts-probe _int]
   [avoid-negative-ts _int]
   [ts-id _int]
   [audio-preload _int]
   [max-chunk-duration _int]
   [max-chunk-size _int]
   [use-wallclock-as-timestamps _int]
   [avio-flags _int]
   [durration-estimation-method _av-duration-estimation-method]
   [skip-initial-bytes _int64]
   [correct-ts-overflow _uint]
   [seek2any _int]
   [flush-packets _int]
   [probe-score _int]
   [format-probesize _int]
   [codec-whitelist _pointer]
   [format-whitelist _pointer]
   [internal _pointer]
   [io-repositioned _int]
   [video-codec _pointer]
   [audio-codec _pointer]
   [subtitle-codec _pointer]
   [data-codec _pointer]
   [metadata-header-padding _int]
   [opaque _pointer]
   [control-message-cb _pointer]
   [output-ts-offset _int64]
   [max-analyze-duration2 _int64]
   [probesize2 _int64]
   [dump-separator _uint8]
   [data-codec-id _avcodec-id]
   [open-cb _pointer]))
(define (avformat-context-streams v)
  (cblock->list (avformat-context-streams-data v)
                _avstream-pointer
                (avformat-context-nb-streams v)))
(define (avformat-context-chapters v)
  (cblock->list (avformat-context-chapters-data v)
                _avchapter-pointer
                (avformat-context-nb-chapters v)))
;; MUST keep key until the GC is allowed to reclaim list.
(define (set-avformat-context-chapters! v ch)
  (define key (list->cblock ch _avchapter))
  (set-avformat-context-nb-chapters! v (length ch))
  (set-avformat-context-chapters-data! v key)
  key)

(define-cstruct _avbuffer-ref
  ([buffer _pointer]
   [data _pointer]
   [size _int]))

(define-cstruct _avpacket
  ([buf _avbuffer-ref-pointer/null]
   [pts _int64]
   [dts _int64]
   [data _pointer]
   [size _int]
   [stream-index _int]
   [flags _int]
   [side-data _pointer]
   [side-data-elems _int]
   [durration _int64]
   [pos _int64]
   [convergence-duration _int64] ;; DEP AVCODEC 59
   ))

(define-cstruct _avpacket-list
  ([pkt _avpacket]
   [next _avpacket-pointer]))

(define-cstruct _avcodec-parameters
  ([codec-type _avmedia-type]
   [codec-id _avcodec-id]
   [codec-tag _uint32]
   [extradata _pointer]
   [extradata-size _int]
   [format _int]
   [bit-rate _int64]
   [bits-per-coded-sample _int]
   [bits-per-raw-sample _int]
   [profile _int]
   [level _int]
   [width _int]
   [height _int]
   [sample-aspect-ration _avrational]
   [field-order _avfield-order]
   [color-range _avcolor-range]
   [color-primaries _avcolor-primaries]
   [color-trc _avcolor-transfer-characteristic]
   [color-space _avcolor-space]
   [chroma-location _avchroma-location]
   [video-delay _int]
   [channel-layout _av-channel-layout]
   [channels _int]
   [sample-rate _int]
   [block-allign _int]
   [frame-size _int]
   [initial-padding _int]
   [trailing-padding _int]
   [seek-preroll _int]))

(define-cstruct _avcodec
  ([name _bytes]
   [long-name _bytes]
   [type _avmedia-type]
   [id _avcodec-id]
   [capabilities _int]
   [supported-framerates _pointer]
   [pix-fmts _pointer]
   [supported-samplerates (_cpointer/null 'int)]
   [sample-fmts _avsample-format-pointer/null]
   [channel-layouts _av-channel-layout-pointer/null]
   [max-lowres _uint8]
   [priv-class _pointer]
   [profiles _pointer]
   [priv-data-size _int]
   [next _pointer]
   [defaults _pointer]
   [init-static-data _fpointer]
   [init _fpointer]
   [encode-sub _fpointer]
   [encode2 _fpointer]
   [decode _fpointer]
   [close* _fpointer]
   [send-frame* _fpointer]
   [send-packet* _fpointer]
   [receive-frame* _fpointer]
   [receive-packet* _fpointer]
   [flush _fpointer]
   [caps-internal _int]
   [init-thread-copy _fpointer]
   [update-thread-context _fpointer]))

(define-cstruct _avcodec-context
  ([av-class _pointer]
   [log-level-offset _int]
   [codec-type* _avmedia-type]
   [codec _avcodec-pointer/null]
   [codec-name (_array _byte 32)] ;; DEP AVCODEC 58
   [codec-id _avcodec-id]
   [codec-tag _uint]
   [stream-codec-tag _uint] ;; DEP AVCODEC 59
   [priv-data _pointer]
   [internal _pointer]
   [opaque _pointer]
   [bit-rate _int64]
   [bit-rate-tolerance _int]
   [global-quality _int]
   [compression-level _int]
   [flags _avcodec-flags]
   [flags2 _avcodec-flags2]
   [extradata _pointer]
   [extradata-size _int]
   [time-base _avrational]
   [ticks-per-frame _int]
   [delay _int]
   [width _int]
   [height _int]
   [coded-width _int]
   [coded-height _int]
   [gop-size _int]
   [pix-fmt _avpixel-format]
   [me-method _int] ;; DEP AVCODEC 59
   [draw-horiz-band _fpointer]
   [get-format _fpointer]
   [max-b-frames _int]
   [b-quant-factor _float]
   [rc-strategy _int] ;; DEP AVCODEC 59
   [b-frame-strategy _int] ;; DEP AVCODEC 59
   [b-quant-offset _float]
   [has-b-frames _int]
   [mpeg-quant _int] ;; DEP AVCODEC 59
   [i-quant-factor _float]
   [i-quant-offset _float]
   [lumi-masking _float]
   [temporal-cpix-masking _float]
   [spatial-cpix-masking _float]
   [p-masking _float]
   [dark-masking _float]
   [slice-count _int]
   [prediction-method _int] ;; DEP AVCODEC 59
   [slice-offset _pointer]
   [sample-aspect-ratio _avrational]
   [me-cmp _int]
   [me-sub-cmp _int]
   [mb-cmp _int]
   [ildct-cmp _int]
   [dia-size _int]
   [last-predictor-count _int]
   [pre-me _int] ;; DEP AVCODEC 59
   [me-pre-cmp _int]
   [pre-dia-size _int]
   [me-subpel-quality _int]
   [dtg-active-format _int] ;; DEP AVCODEC 58
   [me-range _int]
   [intra-quant-bias _int] ;; DEP AVCODEC 59
   [inter-quant-bias _int] ;; DEP AVCODEC 59
   [slice-flags _int]
   [xvmc-acceleration _int] ;; DEP AVCODEC 58
   [mb-decision _int]
   [intra-matrix _pointer]
   [inter-matrix _pointer]
   [scenechange-threashold _int] ;; DEP AVCODEC 59
   [noise-reduction _int] ;; DEP AVCODEC 59
   [me-threashold _int] ;; DEP AVCODEC 59
   [mb-threashold _int] ;; DEP AVCODEC 59
   [intra-dc-precision _int]
   [skip-top _int]
   [skip-bottom _int]
   [border-masking _float] ;; DEP AVCODEC 59
   [mb-lmin _int]
   [mb-lmax _int]
   [me-penalty-compensation _int] ;; DEP AVCODEC 59
   [bidir-refine _int]
   [brd-scale _int] ;; DEP AVCODEC 59
   [keyint-min _int]
   [refs _int]
   [chromaoffset _int] ;; DEP AVCODEC 59
   [scenechange-factor _int] ;; DEP AVCODEC 58
   [mv0-threashold _int]
   [b-sensitivity _int] ;; DEP AVCODEC 59
   [color-primaries _avcolor-primaries]
   [color-trc _avcolor-transfer-characteristic]
   [colorspace _avcolor-space]
   [color-range _avcolor-range]
   [chroma-sample-location _avchroma-location]
   [slices _int]
   [field-order _avfield-order]
   [sample-rate _int]
   [channels _int]
   [sample-fmt _avsample-format]
   [frame-size _int]
   [frame-number _int]
   [block-align _int]
   [cutoff _int]
   [channel-layout _av-channel-layout]
   [request-channel-layout _av-channel-layout]
   [audio-service-type _avaudio-service-type]
   [request-sample-format _avsample-format]
   [get-buffer2 _fpointer]
   [refcounted-frames _int]
   [qcompress _float]
   [qblur _float]
   [qmin _int]
   [qmax _int]
   [max-qdiff _int]
   [rc-qsquish _float]
   [rc-qmod-amp _float]
   [rc-qmod-freq _int]
   [rc-buffer-size _int]
   [rc-override-count _int]
   [rc-override _pointer]
   [rc-eq _bytes]
   [rc-max-rate _int64]
   [rc-min-rate _int64]
   [rc-buffer-aggressivity _float]
   [rc-initial-cpix _float]
   [rc-max-available-vbv-use _float]
   [rc-initial-buffer-occupancy _int]
   [coder-type _int]
   [context-model _int]
   [lmin _int]
   [lmax _int]
   [frame-skip-threshold _int]
   [frame-skip-factor _int]
   [frame-skip-exp _int]
   [frame-skip-cmp _int]
   [trellis _int]
   [min-prediction-order _int]
   [max-prediction-order _int]
   [timecode-frame-start _int64]
   [rtp-callback _fpointer]
   [rtp-payload-size _int]
   [mv-bits _int]
   [header-bits _int]
   [i-tex-bits _int]
   [p-tex-bits _int]
   [i-count _int]
   [p-count _int]
   [skip-count _int]
   [misc-bits _int]
   [frame-bits _int]
   [stats-out _bytes]
   [stats-in _bytes]
   [workaround-bugs _int]
   [strict-std-compliance _int]
   [error-concealment _int]
   [debug _int]
   [debug-mv _int]
   [err-recognition _int]
   [reordered-opaque _int64]
   [hwaccel _pointer]
   [hwaccel-context _pointer]
   [error (_array _uint64 AV-NUM-DATA-POINTERS)]
   [dct-algo _int]
   [idct-algo _int]
   [bits-per-coded-sample _int]
   [bits-per-raw-sample _int]
   [lowres _int]
   [coded-frame _pointer]
   [thread-count _int]
   [thread-type _int]
   [active-thread-type _int]
   [thread-safe-callbacks _int]
   [execute _fpointer]
   [execute2 _fpointer]
   [nsse_weight _int]
   [profile _int]
   [level _int]
   [skip-loop-filter _avdiscard]
   [skip-idct _avdiscard]
   [skip-frames _avdiscard]
   [subtitle-header _pointer]
   [subtitle-header-size _int]
   [error-rate _int]
   [vbv-delay _uint64]
   [side-data-only-packets _int]
   [initial-padding _int]
   [framerate _avrational]
   [sw-pix-fmt _avpixel-format]
   [pkt-timebase _avrational]
   [codec-discriptor _pointer]
   [pts-correction-num-faulty-pts _int64]
   [pts-correction-num-faulty-dts _int64]
   [pts-correction-last-pts _int64]
   [pts-correction-last-dts _int64]
   [sub-charenc _bytes]
   [sub-charenc-mode _int]
   [skip-alpha _int]
   [seek-preroll _int]
   [chroma-intra-matrix _pointer]
   [dump-separator _pointer]
   [codec-whitelist _bytes]
   [properties _uint]
   [coded-side-data _pointer]
   [nb-coded-side-data _int]
   [hw-frames-ctx _pointer]
   [sub-text-format _int]
   [trailling-padding _int]))

(define-cstruct _avprobe-data
  ([filename _bytes]
   [buf _pointer]
   [buf-size _int]
   [mime-type _bytes]))

;; DEP AVFORMAT 58
(define-cstruct _avfrac
  ([val _int64]
   [num _int64]
   [den _int64]))

(define-cstruct _avstream
  ([index _int]
   [id _int]
   [codec _pointer] ;_avcodec-context-pointer] ;; DEP AVFORMAT 58
   [priv-data _pointer]
   [pts _avfrac] ;; DEP AVFORMAT 58
   [time-base _avrational]
   [start-time _int64]
   [duration _int64]
   [nb-frames _int64]
   [disposition _int]
   [discard _avdiscard]
   [sample-aspect-ratio _avrational]
   [metadata _pointer]
   [avg-frame-rate _pointer]
   [attached-pic _avpacket]
   [side-data _pointer]
   [nb-side-data _int]
   [event-flags _int]
   ;; Private
   [info _pointer]
   [pts-wrap-bits _int]
   [first-dts _int64]
   [cur-dts _int64]
   [last-ip-pts _int64]
   [last-ip-duration _int]
   [probe-packets _int]
   [codec-info-nb-frames _int]
   [need-parsing _avstream-parse-type]
   [parser _pointer]
   [last-in-packet-buffer _pointer]
   [probe-data _avprobe-data]
   [pts-buffer (_array _int64 (add1 MAX-REORDER-DELAY))]
   [index-enteries _pointer]
   [nb-index-enteries _int]
   [index-enteries-allocated-size _uint]
   [r-frame-rate _avrational]
   [stream-identifier _int]
   [interleaver-chunk-size _int64]
   [interleaver-chunk-duration _int64]
   [request-probe _int]
   [skip-to-keyframe _int]
   [skip-samples _int]
   [start-skip-samples _int64]
   [first-discard-sample _int64]
   [last-discard-sample _int64]
   [nb-decode-frames _int]
   [mux-ts-offset _int64]
   [pts-wrap-reference _int64]
   [pts-wrap-behavior _int]
   [update-initial-durations-done _int]
   [pts-reorder-error (_array _int64 (add1 MAX-REORDER-DELAY))]
   [pts-reorder-error-count (_array _uint8 (add1 MAX-REORDER-DELAY))]
   [last-dts-for-order-check _int64]
   [dts-ordered _uint8]
   [dts-misordered _uint8]
   [inject-global-side-data _int]
   ;; Public Again
   [recommended-encoder-configuration _string]
   [display-aspect-ration _avrational]
   [priv-pts _pointer]
   [internal _pointer]
   [codecpar _avcodec-parameters-pointer/null]))


;; DEP AVCODEC 59
(define-cstruct _av-picture
  ([data (_array _pointer AV-NUM-DATA-POINTERS)]
   [linesize (_array _int AV-NUM-DATA-POINTERS)]))

(define-cstruct _av-frame-side-data
  ([type _av-frame-side-data-type]
   [data _pointer]
   [size _int]
   [metadata _av-dictionary-pointer]
   [buf _avbuffer-ref-pointer]))

;; The actual avframe struct is much bigger,
;; but only these fields are part of the public ABI.
(define-cstruct _av-frame
  ([data (_array _pointer AV-NUM-DATA-POINTERS)]
   [linesize (_array _int AV-NUM-DATA-POINTERS)]
   [extended-data _pointer]
   [width _int]
   [height _int]
   [nb-samples _int]
   [format _int]
   [key-frame _bool]
   [pict-type _avpicture-type]
   [sample-aspect-ration _avrational]
   [pts _int64]
   [pkt-pts _int64] ;; DEP AVUTIL 56
   [pkt-dts _int64]
   [coded-picture-number _int]
   [display-picture-number _int]
   [quality _int]
   [opaque _pointer]
   [error (_array _uint64 AV-NUM-DATA-POINTERS)] ;; DEP AVUTIL 56
   [repeate-pict _int]
   [interlaced-frame _int]
   [top-field-first _int]
   [palette-has-changed _int]
   [reordered-opaque _int64]
   [sample-rate _int]
   [channel-layout _uint64]
   [buf (_array _pointer AV-NUM-DATA-POINTERS)]
   [extended-buf _pointer]
   [nb-extended-buf _int]
   [side-data* _av-frame-side-data-pointer]
   [nb-side-data _int]
   [flags _av-frame-flags]
   [color-range _avcolor-range]
   [color-primaries _avcolor-primaries]
   [color-trc _avcolor-transfer-characteristic]
   [color-space _avcolor-space]
   [chroma-location _avchroma-location]
   [best-effort-timestamp _int64]
   [pkt-pos _int64]
   [pkt-duration _int64]
   [metadata _pointer]
   [decode-error-flags _ff-decode-error-flags]
   [channels _int]
   [pkt-size _int]
   [qscale-table _int8] ;; DEP AVUTIL 56
   [qstride _int] ;; DEP AVUTIL 56
   [qscale-type _int] ;; DEP AVUTIL 56
   [qp-table-ref _pointer] ;; DEP AVUTIL 56
   [hw-frames-ctx _pointer]
   [opaque-ref _pointer]))
(define (av-frame-format/video frame)
  (cast (av-frame-format frame) _int _avpixel-format))
(define (set-av-frame-format/video! frame format)
  (set-av-frame-format! frame (cast format _avpixel-format _int)))
(define (av-frame-format/audio frame)
  (cast (av-frame-format frame) _int _avsample-format))
(define (set-av-frame-format/audio! frame format)
  (set-av-frame-format! frame (cast format _avsample-format _int)))

(define-cpointer-type _sws-context-pointer)
(define-cpointer-type _swr-context-pointer)

(define-cpointer-type _avfilter-pad-pointer)

(define-cstruct _avfilter
  ([name _string]
   [description _string]
   [inputs _avfilter-pad-pointer/null]
   [outputs _avfilter-pad-pointer/null]
   [priv-class _pointer]
   [flags _avfilter-flags]
   [init _fpointer]
   [init-dict* _fpointer]
   [uninit _fpointer]
   [query-formats _fpointer]
   [priv-size _int]
   [next _avfilter-pointer/null]
   [process-command _fpointer]
   [init-opaque _fpointer]))

(define-cstruct _avfilter-graph-internal
  ([thread _pointer]
   [thread-execute _fpointer]))

(define-cstruct _avfilter-graph
  ([av-class _avclass-pointer/null]
   [filters (_cpointer 'avfilter-pointer-pointer)]
   [nb-filters _int]
   [scale-sws-opts _pointer]
   [resample-lavr-options _pointer]
   [thread-type _int]
   [nb-threads _int]
   [internal* _avfilter-graph-internal-pointer]
   [opaque _pointer]
   [execute _fpointer]
   [aresample-swr-opts _pointer]
   [sink-links _pointer]
   [disable-auto-convert _uint]))

(define-cstruct _avfilter-command
  ([time _double]
   [command _string]
   [arg _string]
   [flags _avfilter-command-flags]
   [next _avfilter-command-pointer]))

(define-cstruct _avfilter-context
  ([av-class _avclass-pointer]
   [filter _avfilter-pointer]
   [name _pointer]
   [input-pads _avfilter-pad-pointer]
   [inputs-data _pointer]
   [nb-inputs _uint]
   [output-pads _avfilter-pad-pointer]
   [outputs-data _pointer]
   [nb-outputs _uint]
   [priv _pointer]
   [commandqueue _avfilter-command-pointer]))

(define-cstruct _avfilter-link
  ([src _avfilter-context-pointer]
   [srcpad _avfilter-pad-pointer]
   [dst _avfilter-context-pointer]
   [dstpad _avfilter-pad-pointer]
   [type _avmedia-type]
   [w _int]
   [h _int]
   [sample-aspect-ratio _avrational]
   [channel-layout _uint64]
   [sample-rate _int]
   [format _int]
   [time-base _avrational]))
(define (avfilter-link-format/video link)
  (cast (avfilter-link-format link) _int _avpixel-format))
(define (set-avfilter-link-format/video! link format)
  (set-avfilter-link-format! link (cast format _avpixel-format _int)))
(define (avfilter-link-format/audio link)
  (cast (avfilter-link-format link) _int _avsample-format))
(define (set-avfilter-link-format/audio! link format)
  (set-avfilter-link-format! link (cast format _avsample-format _int)))

(define (avfilter-context-inputs v)
  (cblock->list (avfilter-context-inputs-data v)
                _avfilter-link-pointer
                (avfilter-context-nb-inputs v)))
(define (avfilter-context-outputs v)
  (cblock->list (avfilter-context-outputs-data v)
                _avfilter-link-pointer
                (avfilter-context-nb-outputs v)))

(define-cstruct _avfilter-in-out
  ([name _pointer]
   [filter-ctx _avfilter-context-pointer]
   [pad-idx _int]
   [next _avfilter-in-out-pointer/null]))

(define-cstruct _av-buffersink-params
  ([pixel-fmts _avpixel-format-pointer/null]))

(define-cstruct _av-buffersink-aparams
  ([sample-fmts _avsample-format-pointer/null]
   [channel-layout _pointer]
   [channel-counts _pointer]
   [all-channel-counts _int]
   [sample-rates _int]))

(define-cstruct _av-buffersrc-parameters
  ([format _int]
   [time-base _avrational]
   [width _int]
   [height _int]
   [sample-aspect-ratio _avrational]
   [frame-rate _avrational]
   [hw-frames-ctx _pointer]
   [sample-rate _int]
   [channel-layout _av-channel-layout]))

(define-cstruct _avsubtitle-rect
  ([x _int]
   [y _int]
   [w _int]
   [h _int]
   [pict _av-picture] ;; DEP AVCODEC 59
   [data (_array _uint8 4)]
   [linesize (_array _int 4)]
   [type _avsubtitle-type]
   [text _pointer]
   [ass _pointer]
   [flags _int]))

(define-cstruct _avsubtitle
  ([format _uint16]
   [start-display-time _uint32]
   [end-display-time _uint32]
   [num-rects _int]
   [rects _pointer]
   [pts _int64]))

(define-cstruct _av-program
  ([id _int]
   [flags _int]
   [discard _avdiscard]
   [stream-index _uintptr]
   [nb-stream-indexes _int]
   [metadata _av-dictionary-pointer]
   [program-num _int]
   [pmt-pid _int]
   [pcr-pid _int]))

(define-ffmpeg-cstruct _av-option
  ([name _string]
   [help _string]
   [offset _int]
   [type _av-option-type]
   [scaler (_union _int64
                   _double
                   _string
                   _avrational)]
   [min _double]
   [max _double]
   [flags _av-option-flags]
   [unit _string]))

(define-cstruct _avdevice-info
  ([device-name _pointer]
   [device-description _pointer]))

(define-cstruct _avdevice-info-list
  ([devices-data _pointer]
   [nb-devices _pointer]
   [default-device _int]))
(define (avdevice-info-list-devices v)
  (cblock->list (avdevice-info-list-devices-data v)
                _avdevice-info-pointer
                (avdevice-info-list-nb-devices v)))

(define-cstruct _avdevice-rect
  ([x _int]
   [y _int]
   [width _int]
   [height _int]))

(define-cstruct _avdevice-capabilities-query
  ([av-class _avclass-pointer/null]
   [device-context _avformat-context-pointer/null]
   [codec _avcodec-id]
   [sample-format _avsample-format]
   [pixel-format _avpixel-format]
   [sample-rate _int]
   [channels _int]
   [channel-layout _int64]
   [window-width _int]
   [window-height _int]
   [frame-width _int]
   [frame-height _int]
   [fps _avrational]))
