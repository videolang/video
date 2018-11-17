#lang racket/base

#|
   Copyright 2016-2018 Leif Andersen

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
         "../log.rkt"
         ffi/unsafe
         racket/format
         racket/set
         racket/list
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/list))

;; Taken from `ffi/unsafe` based on the suggestions from Matthew
;;   Flatt and Ryan Culpepper on Friday, Sept. 22, 2017. This function
;;   was also added to the public API for Racket 2.10.2. This should
;;   be removed from this codebase once we increase the minimum version
;;   to be 6.10.2 (or higher).
(define (compute-offsets* types [alignment #f] [declared '()])
  (unless (and (list? types) (map ctype? types))
    (raise-argument-error 'compute-offsets "(listof ctype?)" types))
  (unless (memq alignment '(#f 1 2 4 8 16))
    (raise-argument-error 'compute-offsets "(or/c #f 1 2 4 8 16)" alignment))
  (unless (and (list? declared) (map (λ (v) (or (not v) (exact-integer? v))) declared))
    (raise-argument-error 'compute-offsets "(listof (or/c exact-integer? #f))" declared))
  (let ([declared (append declared (build-list (- (length types) (length declared)) (λ (n) #f)))])
    (let loop ([ts types] [ds declared] [cur 0] [r '()])
      (if (null? ts)
          (reverse r)
          (let* ([algn (if alignment 
                           (min alignment (ctype-alignof (car ts)))
                           (ctype-alignof (car ts)))]
                 [pos (or (car ds)
                          (+ cur (modulo (- (modulo cur algn)) algn)))])
            (loop (cdr ts)
                  (cdr ds)
                  (+ pos (ctype-sizeof (car ts)))
                  (cons pos r)))))))

(define (build-getter field-name fields types offsets [who #f])
  (define index (index-of fields field-name))
  (if index
      (procedure-rename
       (λ (this)
         (ptr-ref this (list-ref types index) 'abs (list-ref offsets index)))
       (string->symbol (format "~a-~a" who field-name)))
     (λ (this)
       (raise (exn:ffmpeg:lib
               (format "Field ~a in struct ~a not defined in current ffmpeg build" field-name who)
               (current-continuation-marks))))))

(define (build-setter field-name fields types offsets [who #f])
  (define index (index-of fields field-name))
  (if index
      (procedure-rename
       (λ (this val)
         (ptr-set! this (list-ref types index) 'abs (list-ref offsets index) val))
       (string->symbol (format "set-~a-~a!" who field-name)))
      (λ (this val)
        (error who "Field ~a not defined in current ffmpeg build" field-name))))

(begin-for-syntax
  ;; Remove the underscore from id, used for cstructs which
  ;; are defined with an underscore...for reasons...
  ;; Identifier -> String
  (define (remove-underscore id)
    (define name (syntax->datum id))
    (substring (symbol->string name) 1))

  ;; Syntax class for handling the fields of an ffmpeg cstruct.
  (define-syntax-class ffmpeg-cstruct-field
    (pattern [name
              type
              (~or (~optional (~seq #:removed deprecated) #:defaults ([deprecated #'+inf.0]))
                   (~optional (~seq #:added added) #:defaults ([added #'0]))
                   (~optional (~seq #:offset offset) #:defaults ([offset #'#f])))
              ...])))

;; Like define-cstruct, but can add/remove fields depending on versions.
;; Syntax for field is: [name _type #:added vers-exp #:removed vers-exp]
;;   where the `#:added` and `#:removed` versions are optional.
;; Additionally, the cstruct can have overlapping field names
;;   ONLY if they are not used in the same ffmpeg version at the same time.
(define-syntax (define-ffmpeg-cstruct stx)
  (syntax-parse stx
    [(_ _id:id
        (~or (~optional (~seq #:version-proc version-proc)
                        #:defaults ([version-proc #'(λ () (mk-version #:major 0))]))
             (~once (fields:ffmpeg-cstruct-field ...))
             (~optional (~seq #:size size-expr) #:defaults ([size-expr #'#f]))
             (~optional (~seq #:alignment alignment) #:defaults ([alignment #'#f])))
        ...)
     #:with id (format-id stx "~a" (remove-underscore #'_id))
     #:with id-field-names (format-id stx "~a-field-names" #'_id)
     #:with id-pointer (format-id stx "~a-pointer" #'_id)
     #:with id-pointer/null (format-id stx "~a-pointer/null" #'_id)
     #:with id->list (format-id stx "~a->list" #'id)
     #:with make-id (format-id stx "make-~a" #'id)
     #:with id-tag (format-id stx "~a-tag" #'id)
     #:with id? (format-id stx "~a?" #'id)
     #:with ((id-field
              set-id-field!
              field-name) ...) (remove-duplicates
                                #:key (compose syntax->datum first)
                                (for/list ([i (in-list (attribute fields.name))])
                                  (list
                                   (format-id stx "~a-~a" #'id i)
                                   (format-id stx "set-~a-~a!" #'id i)
                                   i)))
     (quasisyntax/loc stx
       (begin
         (define size size-expr)
         (define the-fields
           (list (vector 'fields.name
                         fields.added
                         fields.deprecated
                         fields.type
                         fields.offset)
                 ...))
         (define the-version (with-handlers ([exn:fail?
                                              (λ (e)
                                                (log-video-error "Couldn't find ffmpeg: ~a" e)
                                                0)])
                               (version-major (version-proc))))
         (define current-fields
           (for/list ([i (in-list the-fields)]
                      #:when (and (<= (vector-ref i 1) the-version)
                                  (< the-version (vector-ref i 2))))
             (list (vector-ref i 0) (vector-ref i 3) (vector-ref i 4))))
         (let ([dup? (check-duplicates current-fields
                                       #:key first)])
           (when dup?
             (error 'define-ffmpeg-cstruct "Overlapping definition of ~a" dup?)))
         (define field-names (map first current-fields))
         (define field-types (map second current-fields))
         (define field-manual-offsets (map third current-fields))
         (define offsets (compute-offsets* field-types alignment field-manual-offsets))
         (define-cpointer-type _^TYPE #:tag 'id)
         (define base-id
           (make-cstruct-type field-types #f alignment))
         (define _id
           (make-ctype
            (if size
                (make-union-type base-id (make-array-type _byte size))
                base-id)
            (λ (x)
              (unless (eq? (cpointer-tag x) 'id)
                (error 'id "Not right type, expected: ~a found: ~a"
                       'id
                       (cpointer-tag x)))
              x)
            (λ (x)
              (set-cpointer-tag! x 'id)
              x)))
         (define id-field (build-getter 'field-name field-names field-types offsets 'id)) ...
         (define set-id-field! (build-setter 'field-name field-names field-types offsets 'id)) ...
         (define id-field-names (for/list ([i (in-list the-fields)])
                                  (vector-ref i 0)))
         (define (make-id . inits)
           (define block (malloc _id))
           (for ([n (in-list id-field-names)]
                 [i (in-list inits)]
                 [s! (list set-id-field! ...)]
                 #:when (set-member? field-names n))
             (s! block i))
           (set-cpointer-tag! block 'id)
           block)
         (define id-pointer _^TYPE)
         (define id-pointer/null _^TYPE/null)
         (define id-tag ^TYPE-tag)
         (define id? ^TYPE?)))]))

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

(define-ffmpeg-cstruct _av-dictionary-entry
  ([key _pointer]
   [value _pointer]))

(define-ffmpeg-cstruct _av-dictionary
  ([count _int]
   [elems _av-dictionary-entry-pointer/null]))

(define-ffmpeg-cstruct _avclass
  #:version-proc avutil-version
  ([class-name _bytes]
   [item-name (_fun _pointer -> _string)]
   [option _pointer]
   [version _int]
   [log-level-offset-offset _int]
   [parent-log-context-offset _int]
   [child-next _pointer]
   [child-class-next _pointer] 
   [catagory _avclass-category]
   [get-catagory _fpointer]
   [query-range _fpointer]))

(define-ffmpeg-cstruct _avio-context
  #:version-proc avformat-version
  ([av-class _pointer]
   [buffer _bytes]
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
   [orig-buffer-size _int]
   [short-seek-threashold _int #:added 55]
   [protocol-whitelist _string #:added 55]
   [protocol-blacklist _string #:added 55]
   [write-data-type _pointer #:added 55]
   [ignore-boundary-point _int #:added 55]
   [current-type _avio-data-marker-type #:added 55]
   [last-time _int64 #:added 55]
   [short-seek-get _pointer #:added 55]
   [written _int64 #:added 55]
   [buf-ptr-max _pointer #:added 55]
   [min-packet-size _int #:added 55]))

(define-ffmpeg-cstruct _avio-interrupt-cb
  ([callback _fpointer]
   [opaque _pointer]))

(define-ffmpeg-cstruct _av-input-format
  #:version-proc avformat-version
  ([name _string]
   [long-name _string]
   [flags _avio-format-flags]
   [extensions _string]
   [codec-tag _pointer]
   [priv-class _pointer]
   [mime-type _string]))

(define-ffmpeg-cstruct _av-output-format
  #:version-proc avformat-version
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
  #:version-proc avformat-version
  ([av-class _pointer]
   [iformat _av-input-format-pointer/null]
   [oformat _av-output-format-pointer/null]
   [priv-data _pointer]
   [pb _avio-context-pointer/null]
   [ctx-flags _avformat-context-flags]
   [nb-streams _uint]
   [streams-data _pointer]
   [filename (_array _byte 1024) #:removed avformat-api-format-filename]
   [url _string]
   [start-time _int64]
   [duration _int64]
   [bit-rate _int]
   [packet-size _uint]
   [max-delay _uint]
   [flags _avformat-flags]
   [probesize _uint]           ; Un-deprecated in 57
   [max-analyze-duration _int] ; Un-deprecated in 57
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
   [event-flags _avformat-event-flags]
   [max-ts-probe _int]
   [avoid-negative-ts _avformat-avoid-negative-ts]
   [ts-id _int]
   [audio-preload _int]
   [max-chunk-duration _int]
   [max-chunk-size _int]
   [use-wallclock-as-timestamps _int]
   [avio-flags _avio-flags]
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
   [max-analyze-duration2 _int64 #:removed 57]
   [probesize2 _int64 #:removed 57]
   [dump-separator _uint8]
   [data-codec-id _avcodec-id]
   [open-cb _pointer #:removed 58]
   [protocall-whitelist _pointer #:added 57]
   [io-open _pointer #:added 57]
   [io-close _pointer #:added 57]
   [protocall-blacklist _pointer #:added 57]))
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

(define-ffmpeg-cstruct _avbuffer-ref
  ([buffer _pointer]
   [data _pointer]
   [size _int]))

(define-ffmpeg-cstruct _avpacket
  #:version-proc avcodec-version
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
   [destruct _pointer #:removed avcodec-api-destruct-packet]
   [priv _pointer #:removed avcodec-api-destruct-packet]
   [pos _int64]
   [convergence-duration _int64 #:removed avcodec-api-convergence-duration]))

(define-ffmpeg-cstruct _avpacket-list
  ([pkt _avpacket]
   [next _avpacket-pointer]))

(define-ffmpeg-cstruct _avcodec-parameters
  #:version-proc avcodec-version
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

(define-ffmpeg-cstruct _avcodec
  #:version-proc avcodec-version
  ([name _bytes]
   [long-name _bytes]
   [type _avmedia-type]
   [id _avcodec-id]
   [capabilities _int]
   [supported-framerates _pointer]
   [pix-fmts _avpixel-format-pointer/null]
   [supported-samplerates (_cpointer/null 'int)]
   [sample-fmts _avsample-format-pointer/null]
   [channel-layouts _av-channel-layout-pointer/null]
   [max-lowres _uint8]
   [priv-class _pointer]
   [profiles _pointer]))

(define-ffmpeg-cstruct _avcodec-context
  #:version-proc avcodec-version
  ([av-class _pointer]
   [log-level-offset _int]
   [codec-type* _avmedia-type]
   [codec _avcodec-pointer/null]
   [codec-name (_array _byte 32) #:removed avcodec-api-codec-name]
   [codec-id _avcodec-id]
   [codec-tag _uint]
   [stream-codec-tag _uint #:removed 58]
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
   [me-method _int #:removed avcodec-api-motion-est]
   [draw-horiz-band _fpointer]
   [get-format _fpointer]
   [max-b-frames _int]
   [b-quant-factor _float]
   [rc-strategy _int #:removed avcodec-api-rc-strategy]
   [b-frame-strategy _int #:removed avcodec-api-private-opt]
   [b-quant-offset _float]
   [has-b-frames _int]
   [mpeg-quant _int #:removed avcodec-api-private-opt]
   [i-quant-factor _float]
   [i-quant-offset _float]
   [lumi-masking _float]
   [temporal-cpix-masking _float]
   [spatial-cpix-masking _float]
   [p-masking _float]
   [dark-masking _float]
   [slice-count _int]
   [prediction-method _int #:removed avcodec-api-private-opt]
   [slice-offset _pointer]
   [sample-aspect-ratio _avrational]
   [me-cmp _int]
   [me-sub-cmp _int]
   [mb-cmp _int]
   [ildct-cmp _interlaced-compare]
   [dia-size _int]
   [last-predictor-count _int]
   [pre-me _int #:removed avcodec-api-private-opt]
   [me-pre-cmp _int]
   [pre-dia-size _int]
   [me-subpel-quality _int]
   [dtg-active-format _int #:removed avcodec-api-afd]
   [me-range _int]
   [intra-quant-bias _int #:removed avcodec-api-quant-bias]
   [inter-quant-bias _int #:removed avcodec-api-quant-bias]
   [slice-flags _slice-flags]
   [xvmc-acceleration _int #:removed avcodec-api-xvmc]
   [mb-decision _mb-decision]
   [intra-matrix _pointer]
   [inter-matrix _pointer]
   [scenechange-threashold _int #:removed avcodec-api-private-opt]
   [noise-reduction _int #:removed avcodec-api-private-opt]
   [me-threashold _int #:removed avcodec-api-mpv-opt]
   [mb-threashold _int #:removed avcodec-api-mpv-opt]
   [intra-dc-precision _int]
   [skip-top _int]
   [skip-bottom _int]
   [border-masking _float #:removed avcodec-api-mpv-opt]
   [mb-lmin _int]
   [mb-lmax _int]
   [me-penalty-compensation _int #:removed avcodec-api-private-opt]
   [bidir-refine _int]
   [brd-scale _int #:removed avcodec-api-private-opt]
   [keyint-min _int]
   [refs _int]
   [chromaoffset _int #:removed avcodec-api-private-opt]
   [scenechange-factor _int #:removed avcodec-api-unused-members]
   [mv0-threashold _int]
   [b-sensitivity _int #:removed avcodec-api-private-opt]
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
   [request-channels _int #:removed avcodec-api-request-channels]
   [channel-layout _av-channel-layout]
   [request-channel-layout _av-channel-layout]
   [audio-service-type _avaudio-service-type]
   [request-sample-format _avsample-format]
   [get-buffer _fpointer #:removed avcodec-api-get-buffer]
   [release-buffer _fpointer #:removed avcodec-api-get-buffer]
   [reget-buffer _fpointer #:removed avcodec-api-get-buffer]
   [get-buffer2 _fpointer]
   [refcounted-frames _int] ;; deprecated, no remove API yet
   [qcompress _float]
   [qblur _float]
   [qmin _int]
   [qmax _int]
   [max-qdiff _int]
   [rc-qsquish _float #:removed avcodec-api-mpv-opt]
   [rc-qmod-amp _float #:removed avcodec-api-mpv-opt]
   [rc-qmod-freq _int #:removed avcodec-api-mpv-opt]
   [rc-buffer-size _int]
   [rc-override-count _int]
   [rc-override _pointer]
   [rc-eq _bytes #:removed avcodec-api-mpv-opt]
   [rc-max-rate _int64]
   [rc-min-rate _int64]
   [rc-buffer-aggressivity _float #:removed avcodec-api-mpv-opt]
   [rc-initial-cpix _float #:removed avcodec-api-mpv-opt]
   [rc-max-available-vbv-use _float]
   [rc-min-available-vbv-use _float]
   [rc-initial-buffer-occupancy _int]
   [coder-type _int #:removed avcodec-api-coder-type]
   [context-model _int #:removed avcodec-api-coder-type]
   [lmin _int #:removed avcodec-api-mpv-opt]
   [lmax _int #:removed avcodec-api-mpv-opt]
   [frame-skip-threshold _int #:removed avcodec-api-private-opt]
   [frame-skip-factor _int #:removed avcodec-api-private-opt]
   [frame-skip-exp _int #:removed avcodec-api-private-opt]
   [frame-skip-cmp _int #:removed avcodec-api-private-opt]
   [trellis _int]
   [min-prediction-order _int #:removed avcodec-api-private-opt]
   [max-prediction-order _int #:removed avcodec-api-private-opt]
   [timecode-frame-start _int64 #:removed avcodec-api-private-opt]
   [rtp-callback _fpointer #:removed avcodec-api-rtp-callback]
   [rtp-payload-size _int #:removed avcodec-api-private-opt]
   [mv-bits _int #:removed avcodec-api-stat-bits]
   [header-bits _int #:removed avcodec-api-stat-bits]
   [i-tex-bits _int #:removed avcodec-api-stat-bits]
   [p-tex-bits _int #:removed avcodec-api-stat-bits]
   [i-count _int #:removed avcodec-api-stat-bits]
   [p-count _int #:removed avcodec-api-stat-bits]
   [skip-count _int #:removed avcodec-api-stat-bits]
   [misc-bits _int #:removed avcodec-api-stat-bits]
   [frame-bits _int #:removed avcodec-api-stat-bits]
   [stats-out _bytes]
   [stats-in _bytes]
   [workaround-bugs _workaround-bugs]
   [strict-std-compliance _compliance]
   [error-concealment _error-concealment]
   [debug _debug]
   [debug-mv _int #:removed avcodec-api-debug-mv]
   [err-recognition _int]
   [reordered-opaque _int64]
   [hwaccel _pointer]
   [hwaccel-context _pointer]
   [error (_array _uint64 AV-NUM-DATA-POINTERS)]
   [dct-algo _dct-algorithm]
   [idct-algo _idct-algorithm]
   [bits-per-coded-sample _int]
   [bits-per-raw-sample _int]
   [lowres _int #:removed avcodec-api-lowres]
   [coded-frame _pointer #:removed avcodec-api-coded-frame]
   [thread-count _int]
   [thread-type _int]
   [active-thread-type _int]
   [thread-safe-callbacks _int]
   [execute _fpointer]
   [execute2 _fpointer]
   [thread-opaque _pointer #:removed avcodec-api-thread-opaque]
   [nsse_weight _int]
   [profile _ff-profile]
   [level _int]
   [skip-loop-filter _avdiscard]
   [skip-idct _avdiscard]
   [skip-frames _avdiscard]
   [subtitle-header _pointer]
   [subtitle-header-size _int]
   [error-rate _int #:removed avcodec-api-error-rate]
   [pkt _pointer #:removed avcodec-api-codec-pkt]
   [vbv-delay _uint64 #:removed avcodec-api-vbv-delay]
   [side-data-only-packets _int #:removed avcodec-api-sidedata-only-pkt]
   [initial-padding _int]
   [framerate _avrational]
   [sw-pix-fmt _avpixel-format]
   [pkt-timebase _avrational]
   [codec-discriptor _pointer]
   [lowres _int #:added avcodec-api-lowres]
   [pts-correction-num-faulty-pts _int64]
   [pts-correction-num-faulty-dts _int64]
   [pts-correction-last-pts _int64]
   [pts-correction-last-dts _int64]
   [sub-charenc _bytes]
   [sub-charenc-mode _int]
   [skip-alpha _int]
   [seek-preroll _int]
   [debug-mv* _int #:added avcodec-api-debug-mv]
   [chroma-intra-matrix _pointer]
   [dump-separator _pointer]
   [codec-whitelist _bytes]
   [properties _codec-properties]
   [coded-side-data _pointer #:added 57]
   [nb-coded-side-data _int #:added 57]
   [hw-frames-ctx _pointer #:added 57]
   [sub-text-format _int #:added 57]
   [trailling-padding _int #:added 57]
   [max-pixels _int64 #:added 57]
   [hw-device-context _pointer #:added 57]
   [hwaccell-flags _int #:added 57]
   [apply-cropping _int #:added 57]
   [extra-hw-frames _int #:added 58]))

(define-ffmpeg-cstruct _avprobe-data
  ([filename _bytes]
   [buf _pointer]
   [buf-size _int]
   [mime-type _bytes]))

(define-ffmpeg-cstruct _avfrac
  #:version-proc avformat-version
  ([val _int64]
   [num _int64]
   [den _int64]))

(define-ffmpeg-cstruct _avstream
  #:version-proc avformat-version
  ([index _int]
   [id _int]
   [codec _pointer #:removed avformat-api-lavf-avctx] ;_avcodec-context-pointer]
   [priv-data _pointer]
   [pts _avfrac #:removed avformat-api-lavf-frac]
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
   [r-frame-rate _avrational #:added 58]
   [recommended-encoder-configuration _pointer
                                      #:added 58
                                      #:removed avformat-api-lavf-ffserver]
   [codecpar _avcodec-parameters-pointer/null #:added 58]
   ;; Private (only listed for public fields in 58 and below)
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
   [r-frame-rate _avrational #:removed 58]
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
   ;; Public Again (only for versions less than 58)
   [recommended-encoder-configuration _string #:removed 58]
   [display-aspect-ration _avrational]
   [priv-pts _pointer]
   [internal _pointer]
   [codecpar _avcodec-parameters-pointer/null #:removed 58]))

(define-ffmpeg-cstruct _av-picture
  #:version-proc avcodec-version
  ([data (_array _pointer AV-NUM-DATA-POINTERS)]
   [linesize (_array _int AV-NUM-DATA-POINTERS)]))

(define-ffmpeg-cstruct _av-frame-side-data
  ([type _av-frame-side-data-type]
   [data _pointer]
   [size _int]
   [metadata _av-dictionary-pointer]
   [buf _avbuffer-ref-pointer]))

;; The actual avframe struct is much bigger,
;; but only these fields are part of the public ABI.
(define-ffmpeg-cstruct _av-frame
  #:version-proc avutil-version
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
   [pkt-pts _int64 #:removed avutil-api-pkt-pts]
   [pkt-dts _int64]
   [coded-picture-number _int]
   [display-picture-number _int]
   [quality _int]
   [opaque _pointer]
   [error (_array _uint64 AV-NUM-DATA-POINTERS) #:removed avutil-api-error-frame]
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
   [qscale-table _int8 #:removed avutil-api-frame-qp]
   [qstride _int #:removed avutil-api-frame-qp]
   [qscale-type _int #:removed avutil-api-frame-qp]
   [qp-table-buf _pointer #:removed avutil-api-frame-qp]
   [hw-frames-ctx _pointer]
   [opaque-ref _pointer]
   [crop-top _size]
   [crop-bottom _size]
   [crop-left _size]
   [crop-right _size]))
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

;; TODO, this struct
(define-ffmpeg-cstruct _avsubtitle-rect
  #:version-proc avcodec-version
  ([x _int]
   [y _int]
   [w _int]
   [h _int]
   [pict _av-picture #:removed 59]
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
   [nb-devices _int]
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

(define-ffmpeg-cstruct _av-bprint
  #:size 1024
  ([str _pointer]
   [len _uint]
   [size _uint]
   [size-max _uint]
   [reserved-internal-buffer _pointer]))
