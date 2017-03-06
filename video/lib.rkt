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

(require racket/dict
         racket/class
         racket/contract/base
         racket/port
         racket/math
         "private/video.rkt"
         "private/mlt.rkt"
         "private/utils.rkt"
         "render.rkt"
         (prefix-in list: "render/list.rkt")
         (for-syntax racket/base
                     syntax/parse))

(provide
 (contract-out
  ;; Determine the unedited length of an already converted producer.
  ;;   In and Out cut points are ignored
  [producer-length/unedited (-> any/c nonnegative-integer?)]

  ;; Get the durration of a clip length in a playslit
  [playlist-clip-length (-> any/c nonnegative-integer? nonnegative-integer?)]
  
  ;; Get the start time of a clip in a playlist
  [playlist-clip-start (-> any/c nonnegative-integer? nonnegative-integer?)]

  ;; Get the number of clips in a playlist
  [playlist-clip-count (-> any/c nonnegative-integer?)])

 ;; PROTOTYPE FUNCTION, SHOULD BE MOVED SOMEWHERE ELSE
 avformat-installed-codecs)

(define (producer-length/unedited producer)
  (mlt-producer-get-length (convert producer)))

(define (playlist-clip-length playlist index)
  (mlt-playlist-clip-length (convert playlist) index))

(define (playlist-clip-start playlist index)
  (mlt-playlist-clip-start (convert playlist) index))

(define (playlist-clip-count playlist)
  (mlt-playlist-count (convert playlist)))

(define (avformat-installed-codecs)
  (render (make-producer #:type 'color #:source "black")
          #:render-mixin list:render-mixin))
