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
(require ffi/unsafe
         (except-in ffi/unsafe/define define-ffi-definer)
         ; Remove w/ Racket 6.10:
         ffi-definer-convention
         ; Uncomment w/ Racket 6.10:
         ; ffi/unsafe/define/conventions
         )

(define avcodec-lib (ffi-lib "libavcodec"))
(define-ffi-definer define-avcodec avcodec-lib
  #:make-c-id convention:hyphen->underscore)
(define avformat-lib (ffi-lib "libavformat"))
(define-ffi-definer define-avformat avformat-lib
  #:make-c-id convention:hyphen->underscore)
(define avutil-lib (ffi-lib "libavutil"))
(define-ffi-definer define-avutil avutil-lib
  #:make-c-id convention:hyphen->underscore)
(define swscale-lib (ffi-lib "libswscale"))
(define-ffi-definer define-swscale swscale-lib
  #:make-c-id convention:hyphen->underscore)

;; ===================================================================================================

(define (convert-err err)
  (bytes->string/locale (integer->integer-bytes (abs err) 4 #t)))

(define (MK-TAG [a #\space] [b #\space] [c #\space] [d #\space])
  (integer-bytes->integer (bytes (char->integer a)
                                 (char->integer b)
                                 (char->integer c)
                                 (char->integer d))
                          #t))

(define (FFERRTAG [a #\space] [b #\space] [c #\space] [d #\space])
  (- (MK-TAG a b c d)))

(define AVERROR-EOF (FFERRTAG #\E #\O #\F))
(define AV-NUM-DATA-POINTERS 8)
(define MAX-REORDER-DELAY 16)
(define EAGAIN (lookup-errno 'EAGAIN))
(define EINVAL (lookup-errno 'EINVAL))

(define SWS-BILINEAR 2)

;; Although deprecated, still seems useful
(define AVCODEC-MAX-AUDIO-FRAME-SIZE 192000)

;; ===================================================================================================

(struct exn:ffmpeg exn ())
(struct exn:ffmpeg:again exn:ffmpeg ())
(struct exn:ffmpeg:eof exn:ffmpeg ())

;; ===================================================================================================

(define _avcodec-id (_enum '(none
                             mpeg1video
                             mpeg2video
                             mpeg2video-xvmc ; Depricated and might be removed
                             h261
                             h263
                             rv10
                             rv20
                             mjpeg
                             mjpegb
                             ljpeg
                             sp5x
                             jpegls
                             mpeg4
                             rawvideo
                             msmpeg4v1
                             msmpeg4v2
                             msmpeg4v3
                             wmv1
                             wmv2
                             h263p
                             h263i
                             flv1
                             svq1
                             svq3
                             dvvideo
                             huffyuv
                             cyuv
                             h264
                             indeo3
                             mp2 = #x15000
                             mp3
                             aac
                             ac3
                             dts
                             vorbis
                             dvaudio
                             wmav1
                             wmav2
                             mace3
                             mace6
                             wmdaudio
                             flac
                             mp3adu
                             mp3on4
                             shorten
                             alac
                             westwood-snd1
                             gsm
                             qdm2
                             cook
                             truespeech
                             tta
                             ))) ;; XXX AND MORE! :)
(define _av-duration-estimation-method _fixint)
(define _avmedia-type (_enum '(unknown = -1
                               video
                               audio
                               data
                               subtitle
                               attachment
                               nb)))
(define _avcolor-primaries _fixint)
(define _avpixel-format (_enum '(unknown = -1
                                 yuv420p
                                 yuyv422
                                 rgb24
                                 bgr24
                                 yuv422p
                                 yuv444p
                                 yuv410p
                                 yuv411p
                                 gray8
                                 monowhite
                                 monoblack
                                 pal8
                                 yuvj420p
                                 yuvj422p
                                 yuvj444p
                                 ;xvmc-mpeg2-mc
                                 ;xvmc-mpeg2-idct
                                 uyvy422
                                 uyyvyy411
                                 bgr8
                                 bgr4
                                 bgr4-bytes
                                 rgb8
                                 rgb4
                                 rgb4-byte
                                 nv12
                                 nv21
                                 argb
                                 rgba
                                 abgr
                                 bgra
                                 gray16be
                                 gray16le
                                 yuv440p
                                 yuvj440p
                                 yuva420p
                                 ;vdpau-h264
                                 ;vdpau-mpeg1
                                 ;vdpau-mpeg2
                                 ;vdpau-wmv3
                                 ;vdpau-vc1
                                 rgb48be
                                 rgb48le
                                 rgb565be
                                 rgb565le
                                 rgb555be
                                 rgb555le
                                 vaapi-moco
                                 vaapi-idct
                                 vaapi-vld
                                 yuv420p16le
                                 yuv420p16be
                                 yuv422p16le
                                 yuv422p16be
                                 ;vdpau-mp4
                                 dxva2-vld
                                 rgb444le
                                 rgb444be
                                 bgr444le
                                 bgr444be
                                 ya8
                                 ))) ;; XXX And more! :)
(define _avcolor-transfer-characteristic _fixint)
(define _avcolor-space _fixint)
(define _avcolor-range _fixint)
(define _avchroma-location _fixint)
(define _avfield-order _fixint)
(define _avsample-format _fixint)
(define _avaudio-service-type _fixint)
(define _avdiscard _fixint)
(define _avstream-parse-type _fixint)
(define _avpicture-type _fixint)

;; ===================================================================================================

(define-cstruct _byte-io-context
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
   [is-streamd _int]
   [max-packet-size _int]
   [checksum _ulong]
   [checksum-ptr _pointer]
   [update-checksum _fpointer]
   [error _int]
   [read-pause _fpointer]
   [read-seek _fpointer]))

(define-cstruct _av-io-interrupt-cb
  ([callback _fpointer]
   [opaque _pointer]))

(define (avformat-context-streams v)
  (cblock->list (avformat-context-streams-data v)
                _avstream-pointer
                (avformat-context-streams-nb v)))

(define-cstruct _avformat-context
  ([av-class _pointer]
   [iformat _pointer]
   [oformat _pointer]
   [priv_data _pointer]
   [pb _pointer]
   [ctx-flags _int]
   [streams-nb _uint]
   [streams-data _pointer]
   [filename (_array _byte 1024)]
   [start-time _int64]
   [duration _int64]
   [bit-rate _int]
   [packet-size _uint]
   [max-delay _uint]
   [flags _int]
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
   [chapters _pointer]
   [metadata _pointer]
   [start-time-realtime _int64]
   [fps-probe-size _int]
   [error-recognition _int]
   [interrupt-callback _av-io-interrupt-cb]
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

(define-cstruct _avrational
  ([num _int]
   [den _int]))

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
   [durration _int]
   [destruct _fpointer]
   [priv _pointer]
   [pos _int64]
   [convergence-duration _int64]))

(define-cstruct _avpacket-list
  ([pkt _avpacket]
   [next _avpacket-pointer]))

(define-cstruct _avcodec-context
  ([av-class _pointer]
   [log-level-offset _int]
   [codec-type* _avmedia-type]
   [codec _pointer]
   [codec-name (_array _byte 32)]
   [codec-id _avcodec-id]
   [codec-tag _uint]
   [stream-codec-tag _uint]
   [priv-data _pointer]
   [internal _pointer]
   [opaque _pointer]
   [bit-rate _int64]
   [bit-rate-tolerance _int]
   [global-quality _int]
   [compression-level _int]
   [flags _int]
   [flags2 _int]
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
   [me-method _int]
   [draw-horiz-band _fpointer]
   [get-format _fpointer]
   [max-b-frames _int]
   [b-quant-factor _float]
   [rc-strategy _int]
   [b-frame-strategy _int]
   [b-quant-offset _float]
   [has-b-frames _int]
   [mpeg-quant _int]
   [i-quant-factor _float]
   [i-quant-offset _float]
   [lumi-masking _float]
   [temporal-cpix-masking _float]
   [spatial-cpix-masking _float]
   [p-masking _float]
   [dark-masking _float]
   [slice-count _int]
   [prediction-method _int]
   [slice-offset _pointer]
   [sample-aspect-ratio _avrational]
   [me-cmp _int]
   [me-sub-cmp _int]
   [mb-cmp _int]
   [ildct-cmp _int]
   [dia-size _int]
   [last-predictor-count _int]
   [pre-me _int]
   [me-pre-cmp _int]
   [pre-dia-size _int]
   [me-subpel-quality _int]
   [dtg-active-format _int]
   [me-range _int]
   [intra-quant-bias _int]
   [inter-quant-bias _int]
   [slice-flags _int]
   [mb-decision _int]
   [intra-matrix _pointer]
   [inter-matrix _pointer]
   [scenechange-threashold _int]
   [noise-reduction _int]
   [me-threashold _int]
   [mb-threashold _int]
   [intra-dc-precision _int]
   [xvmc-acceleration _int] ;; Maybe kill?
   [skip-top _int]
   [skip-bottom _int]
   [border-masking _float]
   [mb-lmin _int]
   [mb-lmax _int]
   [me-penalty-compensation _int]
   [bidir-refine _int]
   [brd-scale _int]
   [keyint-min _int]
   [refs _int]
   [chromaoffset _int]
   [scenechange-factor _int]
   [mv0-threashold _int]
   [b-sensitivity _int]
   [color-primaries _avcolor-primaries]
   [color-trc _avcolor-transfer-characteristic]
   [colorspace _avcolor-space]
   [color-range _avcolor-range]
   [chroma-sample-location _avchroma-location]
   [slices _int]
   [field-order _avfield-order]
   [padding _int] ;; XXX: ALMOST CERTAINLY MISSING SOMETHING
   [sample-rate _int]
   [channels _int]
   [sample-fmt _avsample-format]
   [frame-size _int]
   [frame-number _int]
   [block-align _int]
   [cutoff _int]
   [channel-layout _uint64]
   [request-channel-layout _uint64]
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

(define-cstruct _avstream
  ([index _int]
   [id _int]
   [codec _avcodec-context-pointer]
   [priv-data _pointer]
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
   [recommended-encoder-configuration _bytes]
   [display-aspect-ration _avrational]))

(define-cstruct _avcodec
  ([name _bytes]
   [long-name _bytes]
   [type _avmedia-type]
   [id _avcodec-id]
   [capabilities _int]
   [supported-framerates _pointer]
   [pix-fmts _pointer]
   [supported-samplerates _pointer]
   [sample-fmts _pointer]
   [channel-layouts _pointer]
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
   [close _fpointer]
   [send-frame _fpointer]
   [send-packet* _fpointer]
   [receive-frame* _fpointer]
   [receive-packet _fpointer]
   [flush _fpointer]
   [caps-internal _int]
   [init-thread-copy _fpointer]
   [update-thread-context _fpointer]))

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
   [base (_array _pointer AV-NUM-DATA-POINTERS)]
   [sample-aspect-ration _avrational]
   [pts _int64]
   [pkt-pts _int64]
   [pkt-dts _int64]
   [coded-picture-number _int]
   [display-picture-number _int]
   [quality _int]
   [reference _int]
   [qscale-table _pointer]
   [qstride _int]
   [qscale-type* _int]
   [mbskip-table _pointer]
   [motion-val _fpointer]
   [mb-type _pointer]
   [dct-coeff _pointer]
   [ref-index (_array _pointer 2)]
   [opaque _pointer]
   [error (_array _uint64 AV-NUM-DATA-POINTERS)]
   [type _int]
   [repeat_pict _int]
   [interlaced-frame _int]
   [top-field-first _int]
   [palette-has-changed _int]
   [buffer-hints _int]
   [pan-scan _pointer]
   [reordered-opaque _int64]
   [hwaccel-picture-private _pointer]
   [owner _pointer]
   [thread-opaque _pointer]
   [motion-subsample-log2 _uint8]
   [sample-rate _int]
   [channel-layout _uint64]
   [buf (_array _pointer AV-NUM-DATA-POINTERS)]
   [extended-buf _pointer]
   [nb-extended-buf _int]
   [side-data _pointer]
   [nb-side-data _int]
   [flags _int]
   [color-range _avcolor-range]
   [color-primitives _avcolor-primaries]
   [color-trc _avcolor-transfer-characteristic]
   [colorspace _avcolor-space]
   [chroma-location _avchroma-location]
   [best-effort-timestamp _int64]
   [pkt-pos _int64]
   [pkt-duration _int64]
   [metadata _pointer]
   [decode-error-flags _int]
   [channels _int]
   [pkt-size _int]
   [qp-table-buf _int]))
(define-cpointer-type _sws-context-pointer)

;; ===================================================================================================

(define-avformat av-register-all (_fun -> _void))
(define-avformat avformat-open-input (_fun (out : (_ptr io _avformat-context-pointer/null) = #f)
                                           _path
                                           _pointer
                                           _pointer
                                           -> [r : _bool]
                                           -> (let ()
                                                (when r (error "NOO"))
                                                out)))
(define-avformat avformat-find-stream-info (_fun _avformat-context-pointer _pointer
                                                 -> [r : _int]
                                                 -> (let ()
                                                      (when (< r 0) (error "NOO2"))
                                                      (void))))
(define-avformat avformat-close-input (_fun (_ptr i _avformat-context-pointer)
                                            -> _void))
(define-avformat av-dump-format (_fun _avformat-context-pointer _int _path _int
                                      -> _void))
(define (av-read-frame ctx [frame #f])
  (define-avformat av-read-frame (_fun _avformat-context-pointer
                                       [out : _avpacket-pointer]
                                       -> [ret : _int]
                                       -> (cond
                                            [(< ret 0)
                                             (unless frame
                                               (av-packet-unref out))
                                             #f]
                                            [else out])))
  (define frame* (or frame
                     (ptr-ref (malloc _avpacket) _avpacket)))
  (av-read-frame ctx frame*))
(define-avformat av-packet-unref (_fun _avpacket-pointer
                                           -> _void))
(define-avformat av-dup-packet (_fun _avpacket-pointer
                                     -> [ret : _int]
                                     -> (unless (= ret 0)
                                          (error "dup-packet?"))))

(define-avcodec avcodec-find-decoder (_fun _avcodec-id
                                           -> _avcodec-pointer))
(define-avcodec avcodec-alloc-context3 (_fun _avcodec-pointer/null
                                             -> _avcodec-context-pointer))
(define-avcodec avcodec-copy-context (_fun [codec : _?]
                                           [out : _avcodec-context-pointer
                                                = (avcodec-alloc-context3 codec)]
                                           _avcodec-context-pointer
                                           -> [ret : _bool]
                                           -> (let ()
                                                (when ret
                                                  (error "NO3"))
                                                out)))
(define-avcodec avcodec-open2 (_fun _avcodec-context-pointer _avcodec-pointer _pointer
                                    -> [ret : _bool]
                                    -> (when ret
                                         (error "Sigh"))))
(define-avcodec av-image-fill-arrays (_fun (_array _pointer 4)
                                           (_array _int 4)
                                           _pointer ;; XXX FIXME
                                           _avpixel-format
                                           _int
                                           _int
                                           _int
                                           -> [ret : _int]
                                           -> (let ()
                                                (when (< ret 0)
                                                  (error "av-image"))
                                                ret)))
(define-avcodec avcodec-send-packet (_fun _avcodec-context-pointer
                                          (_ptr i _avpacket)
                                          -> [ret : _int]
                                          -> (unless (= ret 0)
                                               (error "FOO"))))
(define-avcodec avcodec-receive-frame (_fun _avcodec-context-pointer
                                            _av-frame-pointer
                                            -> [ret : _int]
                                            -> (cond
                                                 [(= ret 0) (void)]
                                                 [(= (- ret) EAGAIN)
                                                  (raise (exn:ffmpeg:again
                                                          "recev-frame"
                                                          (current-continuation-marks)))]
                                                 [(= ret AVERROR-EOF) eof]
                                                 [else (error 'recev "Error: ~a" (- ret))])))

(define-avutil av-frame-alloc (_fun -> _av-frame-pointer))
(define-avutil av-image-get-buffer-size (_fun _avpixel-format _int _int _int
                                              -> _int))
(define (av-malloc [a #f] [b #f])
  (define type (cond
                 [(ctype? a) a]
                 [(ctype? b) b]
                 [else _byte]))
  (define size (cond
                 [(integer? a) a]
                 [(integer? b) b]
                 [else 1]))
  (define-avutil av-malloc (_fun _size -> (_array type size)))
  (av-malloc (* size (ctype-sizeof type))))

(define-swscale sws-getContext (_fun _int
                                     _int
                                     _avpixel-format
                                     _int
                                     _int
                                     _avpixel-format
                                     _int
                                     _pointer
                                     _pointer
                                     _pointer
                                     -> _sws-context-pointer))
(define-swscale sws-scale (_fun _sws-context-pointer
                                _pointer
                                _pointer
                                _int
                                _int
                                _pointer
                                _pointer
                                -> _int))
