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
(require racket/set
         racket/dict
         racket/list
         ffi/unsafe
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse)
         "lib.rkt"
         "../log.rkt")

(begin-for-syntax
  (define-syntax-class ffmpeg-name
    #:attributes (name added removed)
    (pattern name:id
             #:attr removed #'+inf.0
             #:attr added #'0)
    (pattern (name:id (~or (~optional (~seq #:removed removed) #:defaults ([removed #'+inf.0]))
                           (~optional (~seq #:added added) #:defaults ([added #'0])))
                      ...)))
  (define-splicing-syntax-class ffmpeg-list-item
    #:attributes (item added removed)
    (pattern name:ffmpeg-name
             #:attr item #'(name.name)
             #:attr added #'name.added
             #:attr removed #'name.removed)
    (pattern (~seq name:ffmpeg-name (~datum =) place)
             #:attr item #'(name.name = place)
             #:attr added #'name.added
             #:attr removed #'name.removed)))

(define-syntax-parser _ffmpeg-list
  [(_ (~or (~optional (~seq #:version-proc version-proc)
                      #:defaults ([version-proc #'(λ () (mk-version #:major 0))])))
      items:ffmpeg-list-item ...)
   #'(let ()
       (define the-version (with-handlers ([exn:fail?
                                            (λ (e)
                                              (log-video-error "Couldn't find ffmpeg: ~a" e)
                                              0)])
                             (version-major (version-proc))))
       (append*
        (for/list ([i (in-list `(items.item ...))]
                   [a (in-list (list items.added ...))]
                   [r (in-list (list items.removed ...))]
                   #:when (and (<= a the-version)
                               (< the-version r)))
          i)))])

;; Deprecation flags =================================================================================

(define api-opt-type-metadata 56)
(define api-xvmc 56)
(define api-vdpau 56)

(define api-request-channels 57)
(define api-old-decode-audio 57)
(define api-old-encode-audio 57)
(define api-old-encode-video 57)
(define api-codec-id 57)
(define api-vaapi 57)

(define api-debug-mv 58)
(define api-vima-decoder 58)
(define api-audio-convert 58)
(define api-avcodec-resample api-audio-convert)
(define api-missing-sample 58)
(define api-cap-vdpau 58)
(define api-buffs-vdpaus 58)
(define api-voxware 58)
(define api-set-dimensions 58)
(define api-ac-vlc 58)
(define api-old-msmpeg4 58)
(define api-aspect-extended 58)
(define api-arch-alpha 58)
(define api-unused-members 58)
(define api-idct-xvidmmx 58)
(define api-input-preserved 58)
(define api-normalize-aqp 58)
(define api-gmc 58)
(define api-mv0 58)
(define api-codec-name 58)
(define api-afd 58)
(define api-vismv 58)
(define api-audioenc-delay 58)
(define api-vaapi-context 58)
(define api-merge-sd 58)
(define api-mpv-opt 58)
(define api-stream-codec 58)
(define api-quant-bias 58)
(define api-rc-strategy 58)
(define api-motion-est 58)
(define api-without-prefix 58)
(define api-sidedata-only 58)

(define api-lowres 59)
(define api-avctxtime-base 59)
(define api-coded-frame 59)
(define api-sidedata-only-packet 59)
(define api-vdpau-profile 59)
(define api-convergence-duration 59)
(define api-avpicture 59)
(define api-avpacket-old-api 59)
(define api-rtp-callback 59)
(define api-vbv-delay 59)
(define api-coder-type 59)
(define api-stat-bits 59)
(define api-private-opt 59)
(define api-ass-timing 59)
(define api-old-bsf 59)
(define api-copy-context 59)
(define api-get-context-defaults 59)
(define api-nvenc-old-name 59)
(define api-struct-vaapi-context 59)
(define api-merge-sd-api 59)
(define api-tag-string 59)
(define api-getchroma 59)
(define api-codec-get-set 59)
(define api-user-visible-avhwaccel 59)
(define api-lockmgr 59)
(define api-next 59)

;; ===================================================================================================

(define errno-set
  (set 'E2BIG
       'EACCES
       'EADDRINUSE
       'EADDRNOTAVAIL
       'EAFNOSUPPORT
       'EAGAIN
       'EALREADY
       'EBADF
       'EBADMSG
       'EBUSY
       'ECANCELED
       'ECHILD
       'ECONNABORTED
       'ECONNREFUSED
       'ECONNRESET
       'EDEADLK
       'EDESTADDRREQ
       'EDOM
       'EDQUOT
       'EEXIST
       'EFAULT
       'EFBIG
       'EHOSTUNREACH
       'EIDRM
       'EILSEQ
       'EINPROGRESS
       'EINTR
       'EINVAL
       'EIO
       'EISCONN
       'EISDIR
       'ELOOP
       'EMFILE
       'EMLINK
       'EMSGSIZE
       'EMULTIHOP
       'ENAMETOOLONG
       'ENETDOWN
       'ENETRESET
       'ENETUNREACH
       'ENFILE
       'ENOBUFS
       'ENODATA
       'ENODEV
       'ENOENT
       'ENOEXEC
       'ENOLCK
       'ENOLINK
       'ENOMEM
       'ENOMSG
       'ENOPROTOOPT
       'ENOSPC
       'ENOSR
       'ENOSTR
       'ENOSYS
       'ENOTCONN
       'ENOTDIR
       'ENOTEMPTY
       'ENOTRECOVERABLE
       'ENOTSOCK
       'ENOTSUP
       'ENOTTY
       'ENXIO
       'EOPNOTSUPP
       'EOVERFLOW
       'EOWNERDEAD
       'EPERM
       'EPIPE
       'EPROTO
       'EPROTONOSUPPORT
       'EPROTOTYPE
       'ERANGE
       'EROFS
       'ESPIPE
       'ESRCH
       'ESTALE
       'ETIME
       'ETIMEDOUT
       'ETXTBSY
       'EWOULDBLOCK
       'EXDEV))

(define int->errno-table
  (for/hash ([err (in-set errno-set)])
    (values (lookup-errno err) err)))

(define (convert-err err)
  (cond [(dict-has-key? int->errno-table (abs err))
         (symbol->string (dict-ref int->errno-table (abs err)))]
        [else
         (define ret (integer->integer-bytes (abs err) 4 #t))
         (with-handlers ([exn:fail? (λ (e) ret)])
           (bytes->string/locale ret))]))

(define (MK-TAG [a #\space] [b #\space] [c #\space] [d #\space])
  (integer-bytes->integer (bytes (if (integer? a) a (char->integer a))
                                 (if (integer? a) a (char->integer b))
                                 (if (integer? a) a (char->integer c))
                                 (if (integer? a) a (char->integer d)))
                          #t
                          #f))

(define (MK-BE-TAG [a #\space] [b #\space] [c #\space] [d #\space])
  (integer-bytes->integer (bytes (if (integer? a) a (char->integer a))
                                 (if (integer? a) a (char->integer b))
                                 (if (integer? a) a (char->integer c))
                                 (if (integer? a) a (char->integer d)))
                          #t
                          #f))

(define (FFERRTAG [a #\space] [b #\space] [c #\space] [d #\space])
  (- (MK-TAG a b c d)))

(define AVERROR-BSF-NOT-FOUND     (FFERRTAG #xf8 #\B #\S #\F))
(define AVERROR-EOF               (FFERRTAG #\E  #\O #\F))
(define AVERROR-INVALIDDATA       (FFERRTAG #\I  #\N #\D #\A))
(define AVERROR-BUG               (FFERRTAG #\B  #\U #\G #\!))
(define AVERROR-BUFFER_TOO_SMALL  (FFERRTAG #\B  #\U #\F #\S))
(define AVERROR-EXIT              (FFERRTAG #\E  #\X #\I #\T))
(define AVERROR-STREAM-NOT-FOUND  (FFERRTAG #xf8 #\S #\T #\R))
(define AVERROR-DECODER-NOT-FOUND (FFERRTAG #xf8 #\D #\E #\C))
(define AVERROR-DEMUXER-NOT-FOUND (FFERRTAG #xf8 #\D #\E #\M))
(define AVERROR-ENCODER-NOT-FOUND (FFERRTAG #xf8 #\E #\N #\C))

(define EAGAIN (lookup-errno 'EAGAIN))
(define EINVAL (lookup-errno 'EINVAL))
(define ENOENT (lookup-errno 'ENOENT))
(define EPERM (lookup-errno 'EPERM))
(define ENOMEM (lookup-errno 'ENOMEM))
(define EDOM (lookup-errno 'EDOM))
(define ERANGE (lookup-errno 'ERANGE))

(define AV-NUM-DATA-POINTERS 8)
(define MAX-REORDER-DELAY 16)

(define AVSTREAM-INIT-IN-WRITE-HEADER 0)
(define AVSTREAM-INIT-IN-INIT-OUTPUT 1)

(define SWS-BILINEAR 2)

(define SWR-CH-MAX 16)

(define AV-INPUT-BUFFER-PADDING-SIZE 32)
(define AV-INPUT-BUFFER-MIN-SIZE 16384)

(define AV-NOPTS-VALUE (integer-bytes->integer (integer->integer-bytes #x8000000000000000 8 #f) #t))
(define AV-TIME-BASE 1000000)
(define AV-TIME-BASE-Q (/ 1 AV-TIME-BASE))

;; Although deprecated, still seems useful
(define AVCODEC-MAX-AUDIO-FRAME-SIZE 192000)

;; ===================================================================================================

(define frame-rates
  (hash 'ntsc 30000/1001
        'pal 25/1
        'qntsc 30000/1001
        'qpal 25/1
        'sntsc 30000/1001
        'spal 25/1
        'film 24/1
        'ntsc-film 24000/1001))

(define frame-resolution
  (hash 'ntsc (cons 720 480)
        'pal (cons 720 576)
        'qntsc (cons 352 240)
        'qpal (cons 352 288)
        'sntsc (cons 640 480)
        'spal (cons 768 576)
        'film (cons 352 240)
        'ntsc-film (cons 352 240)
        'sqcif (cons 128 96)
        'qcif (cons 176 144)
        'cif (cons 352 288)
        '4cif (cons 704 576)
        '16cif (cons 1408 1152)
        'qqvga (cons 160 120)
        'qvga (cons 320 240)
        'vga (cons 640 480)
        'svga (cons 800 600)
        'xga (cons 1024 768)
        'uxga (cons 1600 1200)
        'qxga (cons 2048 1536)
        'sxga (cons 1280 1024)
        'qsxga (cons 2560 2048)
        'hsxga (cons 5120 4096)
        'wvga (cons 852 480)
        'wxga (cons 1366 768)
        'wsxga (cons 1600 1024)
        'wuxga (cons 1920 1200)
        'woxga (cons 2560 1600)
        'wqsxga (cons 3200 2048)
        'qsuxga (cons 3840 2400)
        'whsxga (cons 6400 4096)
        'whuxga (cons 7680 4800)
        'cga (cons 320 200)
        'ega (cons 640 350)
        'hd480 (cons 852 480)
        'hd720 (cons 1280 720)
        'hd1080 (cons 1920 1080)
        '2k (cons 2048 1080)
        '2kflat (cons 1998 1080)
        '2kscope (cons 2048 858)
        '4k (cons 4096 2160)
        '4kflat (cons 3996 2160)
        '4kscope (cons 4096 1716)
        'nhd (cons 640 360)
        'hqvga (cons 240 160)
        'wqvga (cons 400 240)
        'fwqvga (cons 432 240)
        'hvga (cons 480 320)
        'qhd (cons 960 540)
        '2kdci (cons 2048 1080)
        '4kdci (cons 4096 2160)
        'uhd2160 (cons 3840 2160)
        'uhd4320 (cons 7680 4320)))

;; ===================================================================================================

(define _ff-decode-error-flags
  (_bitmask `(invalid-bistream
              missing-reference)))

(define _sws-flags
  (_bitmask `(fast-bilinear
              bilinear
              bicubic
              x
              point
              area
              bicublin
              guass
              sinc
              lanczos
              spline)))

(define _avseek-flags
  (_bitmask `(backwards
              byte
              any
              frame)
            _int))

(define _av-channel-layout
  (_bitmask '(front-left = #x1
              front-right = #x2
              front-center = #x4
              low-frequency = #x8
              back-left = #x10
              back-right = #x20
              front-left-of-center = #x40
              front-right-of-center = #x80
              back-center = #x100
              side-left = #x200
              side-right = #x400
              top-center = #x800
              top-front-left = #x1000
              top-front-center = #x2000
              top-front-right = #x4000
              top-back-left = #x8000
              top-back-center = #x10000
              top-back-right = #x20000
              stereo-left = #x20000000
              stereo-right = #x40000000
              wide-left = #x80000000
              wide-right = #x100000000
              surround-direct-left = #x200000000
              surround-direct-right = #x400000000
              low-frequency-2 = #x800000000
              layout-native = #x8000000000000000
              stereo = 3
              mono = 4
              2-point-1 = 11)
            _uint64))
(define-cpointer-type _av-channel-layout-pointer)

(define _avio-format-flags
  (_bitmask '(nofile = #x1
              neednumber = #x2
              show-ids = #x8
              rawpicture = #x20
              globalheader = #x40
              notimestamps = #x80
              generic-index = #x100
              ts-discound = #x200
              variable-fps = #x400
              nodimensions = #x800
              nostreams = #x1000
              nobinsearch = #x2000
              nogensearch = #x4000
              no-byte-seek = #x8000
              allow-flush = #x10000
              ts-nonstrict = #x8020000
              ts-negative = #x40000
              seek-to-pts = #x4000000)
            _int))

(define _avformat-flags
  (_bitmask `(genpts
              ignidx
              nonblock
              igndts
              nofillin
              noparse
              nobuffer
              custom-io
              discard-corrupt
              flush-packet
              bitexact
              mp4a-latm = #x8000
              sort-dts
              priv-opts
              fast-seek
              shortest
              auto-bsf
              ;; timestamp flags
              ts-discount = #x200
              ts-nonstrict = #x20000
              ts-negative = #x40000)))

(define _avformat-context-flags
  (_bitmask `(noheader)
            _int))

(define _avformat-event-flags
  (_bitmask `(metadata-updated)
            _int))

(define _avcodec-flags
  (_bitmask `(unaligned
              qscale
              4mv
              output-corupt
              qpel
              pass1 = #x512
              pass2
              loop-filter
              gray = #x8192
              psnr = ,(arithmetic-shift 1 15)
              truncated
              interlaced-dct = ,(arithmetic-shift 1 18)
              low-delay
              global-header = ,(arithmetic-shift 1 22)
              bitexact
              pred
              interlaced-me = ,(arithmetic-shift 1 29)
              closed-gop = ,(arithmetic-shift 1 31))))

(define _avcodec-flags2
  (_bitmask `(fast
              no-ouput = 4
              local-header
              drop-frame-timecode = ,(arithmetic-shift 1 13)
              chunks = ,(arithmetic-shift 1 15)
              ignore-crop
              show-all = ,(arithmetic-shift 1 22)
              export-mvs = ,(arithmetic-shift 1 28)
              skip-manual)))

(define _avcodec-prop
  (_bitmask `(intral-only
              lossy
              lossless
              reorder
              bitmap-sub = ,(arithmetic-shift 1 16)
              text-sub)))

(define _avio-flags
  (_bitmask `(read = 1
              write = 2
              read-write = 3
              nonblock = 8
              direct = #x8000)))

(define _av-dictionary-flags
  (_bitmask `(match-case
              ignore-suffix
              dont-strdup-key
              dont-strdup-val
              dont-overwrite
              append)))

(define _av-frame-flags
  (_bitmask `(corrupt
              discard = 4)
            _int))

(define _avfilter-flags
  (_bitmask `(dynamic-inputs
              dynamic-outputs
              slice-threads
              support-timeline-generic = ,(arithmetic-shift 1 16)
              support-timeline-internal
              support-timeline = ,(bitwise-ior (arithmetic-shift 1 16)
                                               (arithmetic-shift 1 17)))))

(define _avfilter-command-flags
  (_bitmask `(one
              fast)))

(define _av-buffer-sink-flags
  (_bitmask '(peek
              no-request)))

(define _av-buffer-src-flags
  (_bitmask '(no-check-format = 1
              push = 4
              keep-ref = 8)))

(define _av-opt-flags
  (_bitmask `(encoding-param
              decoding-param
              metadata
              audio-param
              video-param
              subtitle-param
              export
              readonly
              filtering-parma = ,(arithmetic-shift 1 16))
            _int))

(define _av-opt-search-flags
  (_bitmask `(search-children
              search-fake-obj
              allow-null
              multi-component-range = ,(arithmetic-shift 1 12))
            _int))

(define _av-log-flags
  (_bitmask `(skip-repeated
              print-level)))

(define _av-option-flags
  (_bitmask (_ffmpeg-list
             encoding-param
             decoding-param
             [metadata #:removed api-opt-type-metadata]
             audio-param = #x8
             video-param
             export
             readonly
             filtering-param = ,(arithmetic-shift 1 16))))

(define _slice-flags
  (_bitmask `(coded-order
              allow-field
              allow-pane)
            _int))

(define _workaround-bugs
  (_bitmask '(autodetect
              msmpeg4  ; <- Deprecated
              xvid-ilace
              ump4
              no-padding
              amv
              qpel-chroma
              std-qpel
              qpel-chroma2
              direct-blocksize
              edge
              hpel-chroma
              dc-clip
              ms
              truncated)
            _int))

(define _debug
  (_bitmask '(info
              rc
              bitstream
              mb-type
              qp
              mv ; <- deprecated, does nothing
              dct-coeff = #x40
              skip
              startcode
              pts ; <- deprecated
              er
              mmco
              bugs
              vis-qp ; <- deprecated
              vis-mb-type ; <- deprecated
              buffers
              threads
              green-md = #x800000
              nomc = #x1000000)))

(define _codec-properties
  (_bitmask '(lossless
              closed-captions)))

;; ===================================================================================================

(define _is-output (_enum '(input = 0
                            output = 1)))

(define _avcodec-id (_enum  (_ffmpeg-list
                             none
                             
                             ;; Video
                             mpeg1video
                             mpeg2video
                             [mpeg2video-xvmc #:removed api-xvmc]
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
                             vp3
                             theora
                             asv1
                             asv2
                             ffv1
                             4xm
                             vcr1
                             cljr
                             mdec
                             roq
                             interplay-video
                             xan-wc3
                             xan-wc4
                             rpza
                             cinepak
                             ws-vqa
                             msrle
                             msvideo1
                             idcin
                             8bps
                             smc
                             flic
                             truemotion1
                             vmdvideo
                             mszh
                             zlib
                             qtrle
                             tscc
                             ulti
                             qdraw
                             vixl
                             qpeg
                             png
                             ppm
                             pbm
                             pgm
                             pgmyuv
                             pam
                             ffvhuff
                             rv30
                             rv40
                             vc1
                             wmv3
                             loco
                             wnv1
                             aasc
                             indeo2
                             fraps
                             truemotion2
                             bmp
                             cscd
                             mmvideo
                             zmbv
                             avs
                             smackvideo
                             nuv
                             kmvc
                             flashsv
                             cavs
                             jpeg2000
                             vmnc
                             vp5
                             vp6
                             vp6f
                             targa
                             dsicinvideo
                             tiertexseqvideo
                             tiff
                             gif
                             dxa
                             dnxhd
                             thp
                             sgi
                             c93
                             bethsoftvid
                             ptx
                             txd
                             vp6a
                             amv
                             vb
                             pcx
                             sunrast
                             indeo4
                             indeo5
                             mimic
                             rl2
                             escape124
                             dirac
                             bfi
                             cmv
                             motionpixels
                             tgv
                             tgq
                             tqi
                             aura
                             aura2
                             v210x
                             tmv
                             v210
                             dpx
                             mad
                             frwu
                             flashsv2
                             cdgraphics
                             r210
                             anm
                             binkvideo
                             iff-ilbm
                             kgvi
                             yop
                             vp8
                             pictor
                             ansi
                             a64-multi
                             a64-multi5
                             r10k
                             mxpeg
                             lagarith
                             prores
                             jv
                             dfa
                             wmv3image
                             vc1image
                             utvideo
                             bmv-video
                             vble
                             dxtory
                             v410
                             xwd
                             cdxl
                             xbm
                             zerocodec
                             mss1
                             msa1
                             tscc2
                             mts2
                             cllc
                             mss2
                             vp9
                             aic
                             escape130
                             g2m
                             webp
                             hnm4-video
                             hevc
                             fic
                             alias-pix
                             brender-pix
                             paf-video
                             exp
                             vp7
                             sanm
                             sgirle
                             mvc1
                             mvc2
                             hqx
                             tdsc
                             hq-hqa
                             hap
                             dos
                             dxv
                             screenpresso
                             rscc

                             y14p = #x8000
                             avrp
                             012v
                             avui
                             ayuv
                             targa-y216
                             v308
                             v408
                             yuv4
                             avrn
                             cpia
                             xface
                             snow
                             smvjpeg
                             apng
                             daala
                             cfhd
                             truemotion2rt
                             m101
                             magicyuv
                             sheervideo
                             ylc
                             psd
                             pixlet
                             speedhq
                             fmvc
                             scpr
                             clearvideo
                             xpm
                             av1

                             ;; PCM
                             pcm-s16le = #x10000
                             pcm-s16be
                             pcm-u16le
                             pcm-u16be
                             pcm-s8
                             pcm-u8
                             pcm-mulaw
                             pcm-alaw
                             pcm-s32le
                             pcm-s32be
                             pcm-u32le
                             pcm-u32be
                             pcm-s24le
                             pcm-s24be
                             pcm-u24le
                             pcm-u24be
                             pcm-s24daud
                             pcm-zork
                             pcm-s16-planar
                             pcm-dvd
                             pcm-f32be
                             pcm-f32le
                             pcm-f64be
                             pcm-f64le
                             pcm-bluray
                             pcm-lfx
                             s302m
                             pcm-s8-planar
                             pcm-s24le-planar
                             pcm-s32le-planar
                             pcm-s16be-planar

                             pcm-s64le = #x10800
                             pcm-s64be
                             pcm-f16le
                             pcm-f24le

                             ;; ADPCM
                             adpcm-ima-qt = #x11000
                             adpcm-ima-wav
                             adpcm-ima-dk3
                             adpcm-ima-dk4
                             adpcm-ima-ws
                             adpcm-ima-smjpeg
                             adpcm-ms
                             adpcm-4xm
                             adpcm-xa
                             adpcm-adx
                             adpcm-ea
                             adpcm-g726
                             adpcm-ct
                             adpcm-swf
                             adpcm-yamaha
                             adpcm-sbpro-4
                             adpcm-sbpro-3
                             adpcm-sbpro-2
                             adpcm-thp
                             adpcm-ima-amv
                             adpcm-ea-r1
                             adpcm-ea-r3
                             adpcm-ea-r2
                             adpcm-ima-ea-sead
                             adpcm-ima-ea-eacs
                             adpcm-ea-xas
                             adpcm-ea-maxis-xa
                             adpcm-ima-iss
                             adpcm-g722
                             adpcm-ima-apc
                             adpcm-vima

                             adpcm-afc = #x11800
                             adpcm-ima-oki
                             adpcm-dtk
                             adpcm-ima-rad
                             adpcm-g726le
                             adpcm-thp-le
                             adpcm-psx
                             adpcm-aica
                             adpcm-ima-dat4
                             adpcm-mtaf

                             ;; AMR
                             amr-nb = #x12000
                             amr-wb

                             ;; RealAudio
                             ra-144 = #x13000
                             ra-288

                             ;; DPCM Codecs
                             roq-dpcm = #x14000
                             interplay-dpcm
                             xan-dpcm
                             sol-dpcm

                             ;; Audio
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
                             smackaudio
                             qcelp
                             wavpack
                             dsicinaudio
                             imc
                             musepack7
                             mlp
                             gsm-ms
                             atrac3
                             [voxware #:removed api-voxware]
                             ape
                             nellymoser
                             musepack8
                             speex
                             wmavoice
                             wmapro
                             wmalossless
                             atrac3p
                             eac3
                             sipr
                             mp1
                             twinvq
                             truehd
                             mp4als
                             atrac1
                             binkaudio-rdft
                             binkaudio-dct
                             aac-latm
                             qdmc
                             celt
                             g723-1
                             g729
                             8svx-exp
                             8svx-fib
                             bmv-audio
                             ralf
                             iac
                             ilbc
                             opus
                             comfort-noise
                             tak
                             metasound
                             paf-audio
                             on2avc
                             dss-sp

                             ffwavesynth = #x15800
                             sonic
                             sonic-ls
                             evrc
                             smv
                             dsd-lsbf
                             dsd-msbf
                             dsd-lsbf-planar
                             dsd-msbf-planar
                             4gv
                             interplay-acm
                             xma1
                             xma2
                             dst
                             atrac3al
                             atrac3pal  
                             
                             ;; Subtitle
                             dvd-subtitle = #x17000
                             dvb-subtitle
                             text
                             xsub
                             ssa
                             mov-text
                             hdmv-pgs-subtitle
                             dvb-teletext
                             srt

                             microdvd = #x17800
                             eta608
                             jadosub
                             sami
                             realtext
                             stl
                             subviewer1
                             subviewer
                             subrip
                             webvtt 
                             mpl2
                             vplayer
                             pjs
                             ass
                             hdmv-text-subtitle

                             ;; Misc
                             ttf = #x18000
                             scte-35
                             bintext = #x18800
                             xbin
                             idf
                             otf
                             smpte-klv
                             dvd-nav
                             timer-id3
                             bin-data
                             probe = #x19000
                             mpeg2ts = #x20000
                             mpeg4systems
                             ffmetadata = #x21000
                             wrapped-avframe)
                           #:unknown (λ (v)
                                      (string->symbol (format "unkown-format-id-~a" v)))))

(define _av-duration-estimation-method (_enum '(pts
                                                stream
                                                bitrate)))

(define _avmedia-type (_enum '(unknown = -1
                               video
                               audio
                               data
                               subtitle
                               attachment)))

(define _avcolor-primaries _fixint)

(define _avpixel-format (_enum  (_ffmpeg-list
                                 unknown = -1
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
                                 [xvmc-mpeg2-mc #:removed api-xvmc]
                                 [xvmc-mpeg2-idct #:removed api-xvmc]
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
                                 [vdpau-h264 #:removed api-vdpau]
                                 [vdpau-mpeg1 #:removed api-vdpau]
                                 [vdpau-mpeg2 #:removed api-vdpau]
                                 [vdpau-wmv3 #:removed api-vdpau]
                                 [vdpau-vc1 #:removed api-vdpau]
                                 rgb48be
                                 rgb48le
                                 rgb565be
                                 rgb565le
                                 rgb555be
                                 rgb555le
                                 bgr565be
                                 bgr565le
                                 bgr555be
                                 bgr555le
                                 [vaapi-moco #:removed api-vaapi]
                                 [vaapi-idct #:removed api-vaapi]
                                 [vaapi #:added api-vaapi]
                                 yuv420p16le
                                 yuv420p16be
                                 yuv422p16le
                                 yuv422p16be
                                 [vdpau-mpeg4 #:removed api-vdpau]
                                 dxva2-vld
                                 rgb444le
                                 rgb444be
                                 bgr444le
                                 bgr444be
                                 ya8
                                 bgr48be
                                 bgr48le
                                 yuv420p9be
                                 yuv420p9le
                                 yuv420p10be
                                 yuv420p10le
                                 yuv422p10be
                                 yuv422p10le
                                 yuv444p9be
                                 yuv444p9le
                                 yuv444p10be
                                 yuv444p10le
                                 yuv422p9be
                                 yuv422p9le
                                 vda-vld
                                 gbrp
                                 gbrp9be
                                 gbrp9le
                                 gbrp10be
                                 gbrp10le
                                 gbrp16be
                                 gbrp16le
                                 yuva422p
                                 yuva444p
                                 yuva420p9be
                                 yuva420p9le
                                 yuva422p9be
                                 yuva422p9le
                                 yuva444p9be
                                 yuva444p9le
                                 yuva420p10be
                                 yuva420p10le
                                 yuva444p10be
                                 yuva444p10le
                                 yuva420p16be
                                 yuva420p16le
                                 yuva422p16be
                                 yuva422p16le
                                 yuva444p16be
                                 yuva444p16le
                                 vdpau
                                 xyz12le
                                 xyz12be
                                 nv16
                                 nv20le
                                 nv20be
                                 rgba64be
                                 rgba64le
                                 bgra64be
                                 bgra64le
                                 yvyu422
                                 vda
                                 ya16be
                                 ya16le
                                 gbrap
                                 gbrap16be
                                 gbrap16le
                                 qsv
                                 mmal
                                 d3d11va-vld
                                 cuda
                                 0rgb = ,(+ #x123 4)
                                 rgb0
                                 0bgr
                                 bgr0
                                 yuv420p12be
                                 yuv420p12le
                                 yuv420p14be
                                 yuv420p14le
                                 yuv422p12be
                                 yuv422p12le
                                 yuv422p14be
                                 yuv422p14le
                                 yuv444p12be
                                 yuv444p12le
                                 yuv444p14be
                                 yuv444p14le
                                 gbrp12be
                                 gbrp12le
                                 gbrp14be
                                 gbrp14le
                                 yuvj411p
                                 bayer-bggr8
                                 bayer-rggb8
                                 bayer-gbrg8
                                 bayer-grbg8
                                 bayer-bggr16le
                                 bayer-bggr16be
                                 bayer-rggb16le
                                 bayer-rggb16be
                                 bayer-gbrg16le
                                 bayer-gbrg16be
                                 bayer-grbg16le
                                 bayer-grbg16be
                                 xvmc ;; Oddly not DEP. :/
                                 yuv440p10le
                                 yuv440p10be
                                 yuv440p12le
                                 yuv440p12be
                                 ayuv64le
                                 ayuv64be
                                 videotoolbox
                                 p010le
                                 p010be
                                 gbrap12be
                                 gbrap12le
                                 gbrap10be
                                 gbrap10le
                                 mediacodec
                                 gray12be
                                 gray12le
                                 gray10be
                                 gray10le
                                 p016le
                                 p016be
                                 ;;MORE!!!
                                 )
                                _fixint
                               #:unknown (λ (x) (string->symbol (format "other:~a" x)))))
(define-cpointer-type _avpixel-format-pointer)

(define _avcolor-range
  (_enum '(unspecified = 0
           mpeg = 1
           jpeg = 2)))

(define _avcolor-space
  (_enum '(rbp = 0
           bt709
           unspecified
           reserved
           fcc
           bt470bg
           smpte170m
           smpte240m
           ycgco
           bt2020-ncl
           bt2020-cl
           smpte2085)))

(define _avcolor-transfer-characteristic
  (_enum '(reserved0 = 0
           bt709
           unspecified
           reserved
           gamma22
           gamma28
           smpte170m
           smpte240m
           linear
           log
           log-sqrt
           iec61966-2-4
           bt1361-ecg
           iec61966-2-1
           bt2020-10
           bt2020-12
           smpte2048
           smpte428
           arib-std-b67)))

(define _avchroma-location
  (_enum '(unspecified = 0
           left
           center
           topleft
           top
           bottomleft
           bottom)))

(define _avfield-order
  (_enum '(unknown
           progressive
           tt
           bb
           tb
           bt)))

(define _avsample-format
  (_enum '(none = -1
           u8
           s16
           s32
           flt
           dbl
           u8p
           s16p
           s32p
           fltp
           dblp
           s64
           s64p)
         _fixint
         #:unknown (λ (x) (string->symbol (format "other:~a" x)))))
(define-cpointer-type _avsample-format-pointer)

(define _avduration-estimation-method
  (_enum '(from-pts
           from-stream
           from-bitrate)))
  
(define _avaudio-service-type (_enum '(main = 0
                                       effects = 1
                                       visually-impaired = 2
                                       hearing-impaired = 3
                                       dialogue = 4
                                       commentary = 5
                                       emergency = 6
                                       voice-over = 7
                                       karaoke = 8)))

(define _avstream-parse-type (_enum `(none
                                      full
                                      headers
                                      timestamps
                                      full-once
                                      full-raw = ,(MK-TAG 0 #\R #\A #\W))))

(define _avdiscard (_enum '(none = -16
                            default = 0
                            nonref = 8
                            bidir = 16
                            nonintra = 24
                            nonkey = 32
                            all = 48)))

(define _avpicture-type (_enum '(none = 0
                                 I
                                 P
                                 B
                                 S
                                 SI
                                 SP
                                 BI)))

(define _avsubtitle-type (_enum '(none
                                  bitmap
                                  text
                                  ass)))

(define _av-frame-side-data-type (_enum '(panscale
                                          a53-cc
                                          stereo3d
                                          matrixencoding
                                          downmix-info
                                          replayagain
                                          displaymatrix
                                          afd
                                          motion-vectors
                                          skip-samples
                                          audio-service-type
                                          mastering-display-metadata
                                          gop-timecode
                                          spherical)))

(define _av-active-format-description (_enum '(same = 8
                                               4-3
                                               16-9
                                               14-9
                                               4-3-sp-14-9
                                               16-9-sp-14-9
                                               sp-4)))

(define _avclass-category (_enum '(na = 0
                                   input
                                   output
                                   muxer
                                   demuxer
                                   encoder
                                   decoder
                                   filter
                                   bitstream-filter
                                   swscaler
                                   swresampler
                                   device-video-output = 40
                                   device-video-input
                                   audio-output
                                   audio-input
                                   device-output
                                   device-input)))

(define _avfilter-auto-convert
  (_enum '(all = 0
           none = -1)))

(define _av-log-constant
  (_enum `(quiet = -8
           panic = 0
           fatal = 8
           error = 16
           warning = 24
           info = 32
           verbose = 40
           debug = 48
           trace = 56
           max-offset = ,(- 56 -8))
         _fixint
         #:unknown (λ (x) x)))

(define _av-app->dev-message-type
  (_enum `(none = ,(MK-BE-TAG #\N #\O #\N #\E)
           window-size = ,(MK-BE-TAG #\G #\E #\O #\M)
           window-repaint = ,(MK-BE-TAG #\R #\E #\P #\A)
           pause = ,(MK-BE-TAG #\P #\A #\U #\space)
           play = ,(MK-BE-TAG #\P #\L #\A #\Y)
           toggle-pause = ,(MK-BE-TAG #\P #\A #\U #\T)
           set-volume = ,(MK-BE-TAG #\S #\V #\O #\L)
           mute = ,(MK-BE-TAG #\space #\M #\U #\T)
           unmute = ,(MK-BE-TAG #\U #\M #\U #\T)
           toggle-mute = ,(MK-BE-TAG #\T #\M #\U #\T)
           get-volume = ,(MK-BE-TAG #\G #\V #\O #\L)
           get-mute = ,(MK-BE-TAG #\G #\M #\U #\T))))

(define _av-dev->app-message-type
  (_enum `(none = ,(MK-BE-TAG #\N #\O #\N #\E)
           create-window-buffer = ,(MK-BE-TAG #\B #\C #\R #\E)
           prepare-window-buffer = ,(MK-BE-TAG #\B #\P #\R #\E)
           display-window-buffer = ,(MK-BE-TAG #\B #\D #\I #\S)
           buffer-overflow = ,(MK-BE-TAG #\B #\O #\F #\L)
           buffer-underflow = ,(MK-BE-TAG #\B #\U #\F #\L)
           buffer-readable = ,(MK-BE-TAG #\B #\R #\D #\space)
           buffer-writable = ,(MK-BE-TAG #\B #\W #\D #\space)
           mute-state-changed = ,(MK-BE-TAG #\C #\M #\U #\T)
           volume-level-changed = ,(MK-BE-TAG #\C #\V #\O #\L))))

(define _av-option-type
  (_enum `(flags
           int
           int64
           double
           float
           string
           rational
           binary
           dict
           const = 128
           image-size = ,(MK-BE-TAG #\S #\I #\Z #\E)
           pixel-fmt = ,(MK-BE-TAG #\P #\F #\M #\T)
           sample-fmt = ,(MK-BE-TAG #\S #\F #\M #\T)
           video-rate = ,(MK-BE-TAG #\V #\R #\A #\T)
           duration = ,(MK-BE-TAG #\D #\U #\R #\space)
           color = ,(MK-BE-TAG #\C #\O #\L #\R)
           channel-layout = ,(MK-BE-TAG #\C #\H #\L #\A)
           bool = ,(MK-BE-TAG #\B #\O #\O #\L))))

(define _avio-data-marker-type
  (_enum '(header
           sync-point
           boundary-point
           unknown
           trailer)))

(define _avformat-avoid-negative-ts
  (_enum '(auto = -1
           default = 0
           make-non-negative = 1
           make-zero = 2)))

(define _interlaced-compare
  (_enum '(sad = 0
           sse
           satd
           dct
           psnr
           bit
           rd
           zero
           vsad
           vsse
           nsse
           w53
           w97
           dctmax
           dct264
           chroma = 256)))

(define _mb-decision
  (_enum '(simple = 0
           bits
           rd)))

(define _compliance
  (_enum '(experimental = -2
           unofficial = -1
           normal = 0
           strict = 1
           very-strict = 2)
         _int))

(define _error-concealment
  (_enum '(guess-mvs = 1
           deblock = 2
           favor-inter = 256)
         _int))

(define _dct-algorithm
  (_enum '(auto = 0
           fastint
           int
           mmx
           altivec
           faan)))

(define _idct-algorithm
  (_enum '(auto = 0
           int
           simple
           simplemax
           arm
           altivec
           sh4 ; <- deprecated
           simplearm
           ipp = 13 ; <- deprecated
           xvid
           simplearmv5te = 16
           simplearmv6
           simplevis ; <- deprecated
           faan = 20
           simpleneon = 22
           simple-alpha ; <- deprecated
           simpleauto = 128)))

(define _ff-profile
  (let ()
    (define FF_PROFILE_UNKNOWN -99)
    (define FF_PROFILE_RESERVED -100)

    (define FF_PROFILE_AAC_MAIN 0)
    (define FF_PROFILE_AAC_LOW  1)
    (define FF_PROFILE_AAC_SSR  2)
    (define FF_PROFILE_AAC_LTP  3)
    (define FF_PROFILE_AAC_HE   4)
    (define FF_PROFILE_AAC_HE_V2 28)
    (define FF_PROFILE_AAC_LD   22)
    (define FF_PROFILE_AAC_ELD  38)
    (define FF_PROFILE_MPEG2_AAC_LOW 128)
    (define FF_PROFILE_MPEG2_AAC_HE  131)
    
    (define FF_PROFILE_DTS         20)
    (define FF_PROFILE_DTS_ES      30)
    (define FF_PROFILE_DTS_96_24   40)
    (define FF_PROFILE_DTS_HD_HRA  50)
    (define FF_PROFILE_DTS_HD_MA   60)
    (define FF_PROFILE_DTS_EXPRESS 70)
    
    (define FF_PROFILE_MPEG2_422    0)
    (define FF_PROFILE_MPEG2_HIGH   1)
    (define FF_PROFILE_MPEG2_SS     2)
    (define FF_PROFILE_MPEG2_SNR_SCALABLE  3)
    (define FF_PROFILE_MPEG2_MAIN   4)
    (define FF_PROFILE_MPEG2_SIMPLE 5)
    
    (define FF_PROFILE_H264_CONSTRAINED  (arithmetic-shift 1 9))
    (define FF_PROFILE_H264_INTRA        (arithmetic-shift 1 11))
    
    (define FF_PROFILE_H264_BASELINE             66)
    (define FF_PROFILE_H264_CONSTRAINED_BASELINE (bitwise-ior 66 FF_PROFILE_H264_CONSTRAINED))
    (define FF_PROFILE_H264_MAIN                 77)
    (define FF_PROFILE_H264_EXTENDED             88)
    (define FF_PROFILE_H264_HIGH                 100)
    (define FF_PROFILE_H264_HIGH_10              110)
    (define FF_PROFILE_H264_HIGH_10_INTRA        (bitwise-ior 110 FF_PROFILE_H264_INTRA))
    (define FF_PROFILE_H264_HIGH_422             122)
    (define FF_PROFILE_H264_HIGH_422_INTRA       (bitwise-ior 122 FF_PROFILE_H264_INTRA))
    (define FF_PROFILE_H264_HIGH_444             144)
    (define FF_PROFILE_H264_HIGH_444_PREDICTIVE  244)
    (define FF_PROFILE_H264_HIGH_444_INTRA       (bitwise-ior 244 FF_PROFILE_H264_INTRA))
    (define FF_PROFILE_H264_CAVLC_444            44)
    
    (define FF_PROFILE_VC1_SIMPLE   0)
    (define FF_PROFILE_VC1_MAIN     1)
    (define FF_PROFILE_VC1_COMPLEX  2)
    (define FF_PROFILE_VC1_ADVANCED 3)
    
    (define FF_PROFILE_MPEG4_SIMPLE                     0)
    (define FF_PROFILE_MPEG4_SIMPLE_SCALABLE            1)
    (define FF_PROFILE_MPEG4_CORE                       2)
    (define FF_PROFILE_MPEG4_MAIN                       3)
    (define FF_PROFILE_MPEG4_N_BIT                      4)
    (define FF_PROFILE_MPEG4_SCALABLE_TEXTURE           5)
    (define FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION      6)
    (define FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE     7)
    (define FF_PROFILE_MPEG4_HYBRID                     8)
    (define FF_PROFILE_MPEG4_ADVANCED_REAL_TIME         9)
    (define FF_PROFILE_MPEG4_CORE_SCALABLE             10)
    (define FF_PROFILE_MPEG4_ADVANCED_CODING           11)
    (define FF_PROFILE_MPEG4_ADVANCED_CORE             12)
    (define FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE 13)
    (define FF_PROFILE_MPEG4_SIMPLE_STUDIO             14)
    (define FF_PROFILE_MPEG4_ADVANCED_SIMPLE           15)
    
    (define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0   0)
    (define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1   1)
    (define FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION  2)
    (define FF_PROFILE_JPEG2000_DCINEMA_2K              3)
    (define FF_PROFILE_JPEG2000_DCINEMA_4K              4)
    
    (define FF_PROFILE_VP9_0                            0)
    (define FF_PROFILE_VP9_1                            1)
    (define FF_PROFILE_VP9_2                            2)
    (define FF_PROFILE_VP9_3                            3)
    
    (define FF_PROFILE_HEVC_MAIN                        1)
    (define FF_PROFILE_HEVC_MAIN_10                     2)
    (define FF_PROFILE_HEVC_MAIN_STILL_PICTURE          3)
    (define FF_PROFILE_HEVC_REXT                        4)
    'TODO))

(define av-bprint-size-unlimited -1)
(define av-bprint-size-automatic 1)
(define av-bprint-size-count-only 0)
