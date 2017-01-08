#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui/base
         racket/draw
         "editor.rkt")


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
            (define video (new video-editor%))
            (send editor insert
                  (new video-snip%
                       [editor video]
                       [min-width (send video get-min-width)]
                       [min-height (send video get-min-height)])))])))

(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-unit-frame video-frame-mixin)

;(send (get-the-snip-class-list) add video-snip-class)
