#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui
         "editor.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define video-frame-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-insert-menu get-editor)
    (new menu-item%
         [parent (get-insert-menu)]
         [label "Insert Video Editor"]
         [callback
          (λ (i e)
            (define editor (get-editor))
            (define video (new video-editor%))
            (send editor insert
                  (make-object editor-snip% video)))])))

(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-unit-frame video-frame-mixin)
