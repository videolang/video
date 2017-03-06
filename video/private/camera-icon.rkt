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

(provide camera-icon
         camera-button)
(require racket/class
         racket/draw
         racket/gui/base
         images/icons/style
         mrlib/switchable-button
         racket/list)

(define height (toolbar-icon-height))
(define width (* height 2))
(define lens-dim 2/3)
(define camera (make-object bitmap% width height #f #t))

(define dc (make-object bitmap-dc% camera))
(send dc set-pen
      (new pen% [width 0] [color (icon-color->outline-color run-icon-color)]))
(send dc set-brush
      (new brush% [color run-icon-color]))

(define path (new dc-path%))
(send path move-to 0             0)
(send path line-to 0             height)
(send path line-to (* width lens-dim) height)
(send path line-to (* width lens-dim) (* height 3/4))
(send path line-to width         (* height 9/10))
(send path line-to width         (* height 1/10))
(send path line-to (* width lens-dim) (* height 1/4))
(send path line-to (* width lens-dim) 0)
(send path line-to 0             0)
(send dc draw-path path)

(define camera-icon
  (bitmap-render-icon camera))

(define camera-button
  (list
   "Preview Video"
   camera-icon
   (λ (drr-frame)
     (define t (send drr-frame get-definitions-text))
     (define vid-port (open-input-text-editor t 0 'end (λ (s) s) t #t))
     (define program (parameterize ([read-accept-lang #t]
                                    [read-accept-reader #t])
                       (read vid-port)))
     (parameterize ([current-namespace (make-gui-namespace)])
       (define mod-name (second program))
       (eval program)
       (eval `(require ',mod-name))
       (eval '(require video/player))
       (eval '(void (preview vid)))))
   #f))

(require video/base)
