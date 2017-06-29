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
         "private/init.rkt"
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
  ;; Render a video object (including the links
  [render (->* [any/c]
               [(or/c path-string? path? #f)
                #:dest-filename (or/c path-string? path? #f)
                #:render-mixin (or/c (-> class? class?) #f)
                #:profile-name (or/c string? #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps number?
                #:start (or/c nonnegative-integer? #f)
                #:end (or/c nonnegative-integer? #f)
                #:speed (or/c number? #f)
                #:timeout (or/c number? #f)
                #:rendering-box (or/c box? #f)]
               void?)])
 render%
 render<%>)

(define (render video
                [dest #f]
                #:dest-filename [dest-filename #f]
                #:render-mixin [render-mixin values]
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #:start [start #f]
                #:end [end #f]
                #:fps [fps 25]
                #:speed [speed #f]
                #:timeout [timeout #f]
                #:rendering-box [rendering-box #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define r% ((or render-mixin values) render%))
  (define renderer
    (new r%
         [source video]
         [dest-dir dest*]
         [dest-filename dest-filename]
         [width width]
         [height height]
         [start start]
         [end end]
         [fps fps]))
  (send renderer setup)
  (send renderer convert)
  (send renderer render))

(define render<%>
  (interface () setup convert render))

(define render%
  (class* object% (render<%>)
    (super-new)
    (init-field source
                dest-dir
                [dest-filename #f]
                [prof-name #f]
                [width 1920]
                [height 1080]
                [start #f]
                [end #f]
                [fps 25])

    (define render-graph #f)
    (define output-node #f)
    
    (define/public (setup)
      (define file (or dest-filename "out.mp4"))
      (define out-path (path->complete-path (build-path dest-dir file)))
      (set! render-graph (weighted-graph/directed '()))
      (define pad-node
        (mk-filter-node
         (hash 'video (mk-filter "pad"
                                 (hash "width" 1920
                                       "height" 1080))
               'audio (mk-filter "anull"))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph pad-node)
      (define fps-node
        (mk-filter-node
         (hash 'video (mk-filter "fps"
                                 (hash "fps" 25)))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph fps-node)
      (add-directed-edge! render-graph pad-node fps-node 1)
      (define pix-fmt-node
        (mk-filter-node
         (hash 'video (mk-filter "format"
                                 (hash "pix_fmts"
                                       "yuv420p")))
         #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph pix-fmt-node)
      (add-directed-edge! render-graph fps-node pix-fmt-node 1)
      (define sink-node
        (mk-sink-node (stream-bundle->file out-path 'vid+aud)
                      #:counts (hash 'video 1 'audio 1)))
      (add-vertex! render-graph sink-node)
      (add-directed-edge! render-graph pix-fmt-node sink-node 1)
      (set! output-node pad-node))

    (define/public (convert)
      (parameterize ([video:current-render-graph render-graph])
        (define dst (video:convert source))
        (add-directed-edge! render-graph dst output-node 1)))
    
    (define/public (render)
      (ffmpeg:render render-graph))))
