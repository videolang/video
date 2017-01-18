#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui/base
         racket/draw)

(import drracket:tool^)
(export drracket:tool-exports^)

(define-values (video-editor%
                video-snip%
                video-snip-class)
  (with-handlers ([exn:fail? (lambda (e) (values #f #f #f))])
    (values
     (dynamic-require "editor.rkt" 'video-editor%)
     (dynamic-require "editor.rkt" 'video-snip%)
     (dynamic-require "editor.rkt" 'video-snip-class))))

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

(when video-editor%
  (drracket:get/extend:extend-unit-frame video-frame-mixin)
  (send (get-the-snip-class-list) add video-snip-class))
