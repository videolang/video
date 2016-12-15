#lang racket/base

(provide video-editor%)
(require data/gvector
         racket/class
         racket/gui/base
         racket/match
         racket/list
         pict
         images/icons/style)

(define video-editor%
  (class pasteboard%
    (init-field [track-height 200]
                [draw-background? #t])
    (super-new)
    (define adjusting-clip? #f)
    (match-define-values (track-width _) (send this get-max-view-size))
    (define frames-per-pixel 1)
    (define line-frequency 25)
    (define line-number-frequency 4)
    (define track-pict #f)
    (define ruler-height 100)
    
    (define (update-track-pict!)
      (set! track-pict
            (bitmap-render-icon
             (pict->bitmap (filled-rectangle track-width track-height
                                             #:color "white"))
             3/10)))
    (update-track-pict!)
    
    (define (round-to-nearest-track n)
      (let* ([acc (/ n track-height)]
             [acc (round acc)]
             [acc (* acc track-height)])
        acc))

    (define/augment (after-move-to s x y d)
      (unless (or d adjusting-clip?)
        (dynamic-wind
         (λ () (set! adjusting-clip? #t))
         (λ ()
           (send this move-to s x (round-to-nearest-track y)))
         (λ () (set! adjusting-clip? #f)))))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when (and before? draw-background?)
        (define-values (local-dx local-dy)
          (send this dc-location-to-editor-location dx dy))
        (define-values (local-0x local-0y)
          (send this dc-location-to-editor-location 0 0))
        (define-values (width height)
          (send this get-max-view-size))
        (unless (= width track-width)
          (set! track-width width)
          (update-track-pict!))
        (for ([i (in-naturals)]
              #:break (>= (+ (* i track-height) local-dy) height))
          (send dc draw-bitmap track-pict (- local-0x) (- (+ (* i track-height) local-dy) local-0y)))
        (define possible-ruler-length
          (- width local-dx))
        (void))) ;; TODO: Draw frames

    (define/augment (on-insert snip before x y)
      (send snip resize 100 track-height))

    (define/override (interactive-adjust-resize s w h)
      (set-box! h track-height)
      (super interactive-adjust-resize s w h))

    (define (make-right-click-menu x y)
      (define p (new popup-menu%))
      (new menu-item%
           [parent p]
           [label "Insert Text"]
           [callback (λ (item event)
                       (define t (new text%))
                       (send t set-max-undo-history 100)
                       (send t insert "Hello World")
                       (send this insert
                             (make-object editor-snip% t)
                             x
                             (round-to-nearest-track y)))])
      (new menu-item%
           [parent p]
           [label "Insert Graphical"]
           [callback (λ (item event)
                       (define pb (new video-editor%
                                       [track-height (/ track-height 4)]))
                       (send this insert
                             (make-object editor-snip% pb)
                             x
                             (round-to-nearest-track y))
                       (define t (new text%))
                       (send t set-max-undo-history 100)
                       (send t insert "Hello World")

                       (send pb insert (make-object editor-snip% t) 0 0)
                       )])
      (new menu-item%
           [parent p]
           [label "Delete Video"]
           [callback (λ (item event)
                       (void))])
      p)

    (define/override (on-default-event event)
      (match (send event get-event-type)
        ['right-down
         (define admin (send this get-admin))
         (define x (send event get-x))
         (define y (send event get-y))
         (when admin
           (send admin popup-menu (make-right-click-menu x y) x y))]
        [else (super on-default-event event)]))))
