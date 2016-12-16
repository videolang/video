#lang racket/base

(provide video-editor%)
(require data/gvector
         racket/class
         racket/gui/base
         racket/match
         racket/list
         pict
         framework
         images/icons/style)

(define video-editor%
  (class pasteboard%
    (init-field [track-height 200]
                [draw-background? #t]
                [initial-tracks 3])
    (super-new)
    (define adjusting-clip? #f)
    (match-define-values (track-width _) (send this get-max-view-size))
    (define frames-per-pixel 1)
    (define line-frequency 25)
    (define line-number-frequency 4)
    (define track-pict #f)
    (define ruler-height 100)

    ;; Hash[Snip, (track)Integer]
    (define snip-table (make-hasheq))

    ;; GVector[Hash[Snip, (start)Integer]]
    (define tracks (vector->gvector (build-vector initial-tracks
                                                  (λ (i) (make-hasheq)))))

    (send this set-min-height (* (gvector-count tracks) track-height))

    ;; -> Void
    (define (update-track-pict!)
      (set! track-pict
            (bitmap-render-icon
             (pict->bitmap (filled-rectangle track-width track-height
                                             #:color "white"))
             3/10)))
    (update-track-pict!)

    ;; Determine relevent Y position for the track
    ;; (pixel) Number -> (pixel) Number
    (define (round-to-nearest-track n)
      (* (position->track n) track-height))

    ;; Determines which track an object should be in
    ;;   given it's Y-value
    ;; (pixel) Number -> (track) Number
    (define (position->track n)
      (let* ([acc (min n (* (- (gvector-count tracks) 1) track-height))]
             [acc (max acc 0)]
             [acc (/ acc track-height)]
             [acc (round acc)])
        acc))

    ;; Determine the pixel position of an object given
    ;;    the track it is on
    ;; (track) Number -> (pixel) Number
    (define (track->position n)
      (* n track-height))

    ;; Update the internal representation of the editor
    ;;   to move a video to the correct track
    ;; Snip% Integer Integer -> Void
    (define (move-video-to-track video track#* position)
      (define track# (inexact->exact track#*))
      (define current-track# (hash-ref snip-table video #f))
      (when current-track#
        (define current-track (gvector-ref tracks current-track#))
        (hash-remove! current-track video))
      (hash-set! snip-table video track#)
      (define new-track (gvector-ref tracks track#))
      (hash-set! new-track video position))
      
    (define/augment (after-move-to s x y d)
      (unless (or d adjusting-clip?)
        (dynamic-wind
         (λ () (set! adjusting-clip? #t))
         (λ ()
           (define track (position->track y))
           (move-video-to-track s track x)
           (displayln tracks)
           (send this move-to s x (track->position track)))
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
        (for ([i (in-range (gvector-count tracks))])
          (send dc draw-bitmap
                track-pict
                (- local-0x)
                (- (+ (* i track-height) local-dy) local-0y)))))

    ;; Append a new track to the end of the gvector
    ;; -> Void
    (define (add-track)
      (gvector-add! tracks (make-hasheq))
      (send this set-min-height (* (gvector-count tracks) track-height))
      
      (send this invalidate-bitmap-cache))

    (define/augment (on-insert snip before x y)
      (send snip resize 100 track-height))

    (define/augment (on-delete snip)
      (define current-track (hash-ref snip-table snip))
      (hash-remove! snip-table snip)
      (define track-table (gvector-ref tracks current-track))
      (hash-remove! track-table snip))

    (define/override (interactive-adjust-resize s w h)
      (set-box! h track-height)
      (super interactive-adjust-resize s w h))

    (define (insert-video video track position)
      (define vid-snip (make-object editor-snip% video))
      (send this insert vid-snip position (track->position track))
      (move-video-to-track vid-snip track position))

    (define (make-right-click-menu x y)
      (define p (new popup-menu%))
      (new menu-item%
           [parent p]
           [label "Play"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Add Track"]
           [callback (λ (item event)
                       (add-track))])
      (new menu-item%
           [parent p]
           [label "Delete Track"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Insert Video from File"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new menu-item%
           [parent p]
           [label "Insert Text"]
           [callback (λ (item event)
                       (define t (new video:text%
                                      [track-height track-height]))
                       (send t set-max-undo-history 100)
                       (insert-video t (position->track y) x))])
      (new menu-item%
           [parent p]
           [label "Insert Graphical"]
           [callback (λ (item event)
                       (define pb (new video-editor%
                                       [track-height (/ track-height 4)]))
                       (insert-video pb (position->track y) x))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Zoom in"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new menu-item%
           [parent p]
           [label "Zoom Out"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Delete Video"]
           [callback (λ (item event)
                       (send this delete))])
      p)

    (define/override (on-default-event event)
      (match (send event get-event-type)
        ['right-down
         (define admin (send this get-admin))
         (define x (send event get-x))
         (define y (send event get-y))
         (define-values (local-x local-y)
           (send this dc-location-to-editor-location x y))
         (when admin
           (send admin popup-menu
                 (make-right-click-menu local-x local-y)
                 local-x local-y))]
        [_ (super on-default-event event)]))))

(define video:text%
  (class racket:text%
    (init-field [track-height 100])
    (super-new)
    
    (define (make-right-click-menu x y)
      (define p (new popup-menu%))
      (new menu-item%
           [parent p]
           [label "Play"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Insert Graphical"]
           [callback (λ (item event)
                       (define pb (new video-editor%
                                       [track-height track-height]))
                       (send this insert (make-object editor-snip% pb)))])
      (new separator-menu-item% [parent p])
      (new menu-item%
           [parent p]
           [label "Delete Video"]
           [callback (λ (item event)
                       (send this delete))])
      p)
    
    (define/override (on-event event)
      (match (send event get-event-type)
        ['right-down
                  (define admin (send this get-admin))
         (define x (send event get-x))
         (define y (send event get-y))
         (define-values (local-x local-y)
           (send this dc-location-to-editor-location x y))
         (when admin
           (send admin popup-menu
                 (make-right-click-menu local-x local-y)
                 local-x local-y))]
        [_ (super on-event event)]))))
