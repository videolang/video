#lang racket/unit

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

(require drracket/tool
         framework
         racket/class
         racket/gui/base
         racket/draw
         ffi/unsafe)

(import drracket:tool^)
(export drracket:tool-exports^)

(define video-frame-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-insert-menu
             get-editor
             get-button-panel
             register-toolbar-button)
    (new menu-item%
         [parent (get-insert-menu)]
         [label "Insert Video Editor"]
         [callback
          (λ (i e)
            (define editor (get-editor))
            (define video (new (dynamic-require 'video/private/editor 'video-editor%)))
            (send editor insert
                  (new (dynamic-require 'video/private/editor 'video-snip%)
                       [editor video])))])))

(define (phase1) (void))
(define (phase2) (void))


;; Very large hack, only run if libmlt is found.
;; Also should really not use a copy/pasted ffi-lib line.
(when (ffi-lib "libmlt" '("6") #:fail (λ () #f))
  (drracket:get/extend:extend-unit-frame video-frame-mixin)
  (send (get-the-snip-class-list) add (dynamic-require 'video/private/editor 'video-snip-class)))
