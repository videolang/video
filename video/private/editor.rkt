#lang racket/base

(provide (all-defined-out)
         (rename-out [video-snip-class snip-class]
                     [video-snip-reader reader]))

(require data/gvector
         racket/dict
         racket/class
         racket/gui/base
         racket/match
         racket/list
         racket/format
         racket/path
         file/convertible
         (except-in pict frame blank clip)
         (prefix-in pict: pict)
         framework
         wxme
         images/icons/style
         "../base.rkt")

(struct video-prop (start
                    length)
  #:transparent)

;; Video Editors are the graphical NLVEs that can be placed in
;;    DrRacket
(define video-editor%
  (let ()
    (define-local-member-name
      adjusting-clip?
      track-width
      frames-per-pixel
      line-frequency
      line-number-frequency
      track-pict
      ruler-pict
      ruler-height
      snip-table
      tracks
      update-track-pict!)
    (class* pasteboard% (readable<%>)
      (init-field [track-height 150]
                  [draw-background? #t]
                  [minimum-width 600]
                  [initial-tracks 3]
                  [initial-snip-length 100])
      (super-new)
      (field [adjusting-clip? (make-parameter #f)]
             [track-width (let-values ([(w h) (send this get-max-view-size)])
                            w)]
             [frames-per-pixel 1]
             [line-frequency 25]
             [line-number-frequency 4]
             [ruler-height 25]
             [track-pict #f]
             [ruler-pict #f]
             ;; Hash[Snip, (track)Integer]
             [snip-table (make-hasheq)]
             ;; GVector[Hash[Snip, VideoProp[(start)Integer, (length)Integer]]
             [tracks (vector->gvector (build-vector initial-tracks
                                                    (λ (i) (make-hasheq))))])
      (send this set-min-height (* (gvector-count tracks) track-height))
      (send this set-min-width minimum-width)

      ;; -> Void
      (define/public (update-track-pict!)
        (set! track-pict
              (bitmap-render-icon
               (pict->bitmap (filled-rectangle track-width track-height
                                               #:color "white"))
               3/10))
        (define line-count (inexact->exact (+ 1 (floor (/ track-width line-frequency)))))
        (define lines (apply hc-append line-frequency
                             (make-list line-count (vline 0 ruler-height))))
        (define number-count (inexact->exact (floor (/ line-count line-number-frequency))))
        (set! ruler-pict
              (vl-append
               (apply hc-append
                      (build-list number-count
                                  (λ (n) (lc-superimpose
                                          (pict:blank (* line-frequency line-number-frequency) 1)
                                          (text (number->string (* frames-per-pixel
                                                                   line-frequency
                                                                   line-number-frequency
                                                                   n)))))))
               lines)))
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
               [acc (floor acc)]
               [acc (inexact->exact acc)])
          acc))

      ;; Determine the pixel position of an object given
      ;;    the track it is on
      ;; (track) Number -> (pixel) Number
      (define (track->position n)
        (* n track-height))

      ;; Sets the hight of a track
      ;; Number -> Void
      (define/public (set-track-height! new-height)
        (set! track-height new-height)
        (send this set-min-height (* (gvector-count tracks) track-height))
        (for ([track (in-gvector tracks)]
              [t# (in-naturals)])
          (for ([(snip prop) (in-hash track)])
            (parameterize ([adjusting-clip? #t])
              (send this move-to snip (video-prop-start prop) (track->position t#)))
            (send snip resize (video-prop-length prop) track-height)))
        (update-track-pict!)
        (send this invalidate-bitmap-cache))

      ;; Update the internal representation of the editor
      ;;   to move a video to the correct track
      ;; Snip% Integer (U Integer #f) (U Integer #f) -> Void
      (define (move-video-to-track video track#* [position #f] [length #f])
        (define track# (inexact->exact track#*))
        (define current-track# (hash-ref snip-table video #f))
        (when current-track#
          (define current-track (gvector-ref tracks current-track#))
          (define current-prop (hash-ref current-track video))
          (unless position
            (set! position (video-prop-start current-prop)))
          (unless length
            (set! length (video-prop-length current-prop)))
          (hash-remove! current-track video))
        (hash-set! snip-table video track#)
        (define new-track (gvector-ref tracks track#))
        (hash-set! new-track video (video-prop position length)))

      (define/augment (after-resize s w h r?)
        (when r?
          (define track# (hash-ref snip-table s))
          (define track (gvector-ref tracks track#))
          (define start-time (video-prop-start (hash-ref track s)))
          (hash-set! track s (video-prop start-time w))))
      
      (define/augment (after-move-to s x y d)
        (unless (or d (adjusting-clip?))
          (parameterize ([adjusting-clip? #t])
            (define track (position->track y))
            (move-video-to-track s track x)
            (send this move-to s x (track->position track)))))

      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (define-values (local-dx local-dy)
          (send this dc-location-to-editor-location dx dy))
        (define-values (local-0x local-0y)
          (send this dc-location-to-editor-location 0 0))
        (define-values (width height)
          (send this get-max-view-size))
        (when (and before? draw-background?)
          ;; Weird to re-render the background on paint
          ;; is it possible to do it when editor is resized?
          (unless (= width track-width)
            (set! track-width width)
            (update-track-pict!))
          (for ([i (in-range (gvector-count tracks))])
            (send dc draw-bitmap
                  track-pict
                  (- local-0x)
                  (- (+ (* i track-height) local-dy) local-0y))))
        (when (and (not before?) draw-background?)
          (send dc draw-bitmap
                (pict->bitmap ruler-pict)
                (- local-0x)
                (- (* (gvector-count tracks) track-height) (pict-height ruler-pict) local-0y))))

      ;; Set the number of tracks stored in the editor
      ;; WARNING: WIPES OUT ALL EXISTING CLIPS!
      (define/public (set-tracks! track-count)
        (set! snip-table (make-hasheq))
        (set! tracks (vector->gvector (build-vector
                                       track-count
                                       (λ (i) (make-hasheq)))))
        (send this set-min-height (* (gvector-count tracks) track-height))
        (send this invalidate-bitmap-cache))


      ;; Append a new track to the end of the gvector
      ;; -> Void
      (define (add-track)
        (gvector-add! tracks (make-hasheq))
        (send this set-min-height (* (gvector-count tracks) track-height))
        (send this invalidate-bitmap-cache))

      ;; Delete all snips in the current track, and move all snips below it up one
      ;; Integer -> Void
      (define (delete-track track#)
        (define track (gvector-ref tracks track#))
        (for ([vid (in-list (hash-keys snip-table))])
          (define t (hash-ref snip-table vid))
          (when (= t track#)
            (send this delete vid))
          (when (> t track#)
            (hash-update! snip-table vid sub1)
            (send this move vid 0 (- track-height))))
        (gvector-remove! tracks track#)
        (send this set-min-height (* (gvector-count tracks) track-height))
        (send this invalidate-bitmap-cache))

      (define/augment (on-insert snip before x y)
        (define track# (hash-ref snip-table snip #f))
        (define snip-len
          (let/ec use-default
            (unless track#
              (use-default initial-snip-length))
            (define track (gvector-ref tracks track#))
            (define vprop (hash-ref track snip))
            (define length (video-prop-length vprop))
            (unless length
              (use-default initial-snip-length))
            length))
        (send snip resize snip-len track-height))
      
      (define/augment (on-delete snip)
        (define current-track (hash-ref snip-table snip))
        (hash-remove! snip-table snip)
        (define track-table (gvector-ref tracks current-track))
        (hash-remove! track-table snip))

      ;; Overwritten to prevent changing snips height.
      ;;   (It should always be the track height.)
      (define/override (interactive-adjust-resize s w h)
        (set-box! h track-height)
        (super interactive-adjust-resize s w h))

      ;; Insert a video into the track
      ;; (U VideoSnip ViedoEditor) Integer Integer Integer -> Void
      (define/public (insert-video video track position length)
        (define vid-snip
          (cond [(is-a? video snip%)
                 video]
                [else (make-object video-snip% video)]))
        (move-video-to-track vid-snip track position length)
        (send this insert vid-snip position (track->position track))
        (send this resize vid-snip length track-height))

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
                         (define current-track (position->track y))
                         (delete-track current-track))])
        (new separator-menu-item% [parent p])
        (new menu-item%
             [parent p]
             [label "Insert Video from File"]
             [callback (λ (item event)
                         (define file (get-file))
                         (define t (new video-file%
                                        [file file]
                                        [track-height track-height]))
                         (insert-video t (position->track y) x initial-snip-length))])
        (new menu-item%
             [parent p]
             [label "Insert Text"]
             [callback (λ (item event)
                         (define t (new video-text%
                                        [track-height track-height]))
                         (send t set-max-undo-history 100)
                         (insert-video t (position->track y) x initial-snip-length))])
        (new menu-item%
             [parent p]
             [label "Insert Graphical"]
             [callback (λ (item event)
                         (define pb
                           (new video-editor%
                                [track-height (/ track-height initial-tracks)]
                                [initial-tracks initial-tracks]
                                [initial-snip-length (/ initial-snip-length initial-tracks)]))
                         (insert-video pb (position->track y) x initial-snip-length))])
        (new separator-menu-item% [parent p])
        (new menu-item%
             [parent p]
             [label "Zoom In"]
             [callback (λ (item event)
                         (error "TODO"))])
        (new menu-item%
             [parent p]
             [label "Zoom Out"]
             [callback (λ (item event)
                         (error "TODO"))])
        (new menu-item%
             [parent p]
             [label "Taller Tracks"]
             [callback (λ (item event)
                         (set-track-height! (* track-height 3/2)))])
        (new menu-item%
             [parent p]
             [label "Shorter Tracks"]
             [callback (λ (item event)
                         (set-track-height! (* track-height 2/3)))])
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
          [_ (super on-default-event event)]))

      (define/override (copy-self)
        (define vid (new video-editor%
                         [track-height track-height]
                         [draw-background? draw-background?]
                         [minimum-width minimum-width]
                         [initial-tracks initial-tracks]))
        (copy-self-to vid)
        vid)

      (define/override (copy-self-to dest)
        ;(super copy-self-to dest)
        ;(send dest select-all) ;; hack because this also copies all
        ;(send dest clear)      ;;    of our snips
        (set-field! track-width dest track-width)
        (set-field! frames-per-pixel dest frames-per-pixel)
        (set-field! line-frequency dest line-frequency)
        (set-field! track-pict dest track-pict)
        (set-field! ruler-height dest ruler-height)
        (set-field! track-height dest track-height)
        (send dest set-tracks! (gvector-count tracks))
        (for ([track (in-gvector tracks)]
              [i (in-naturals)])
          (for ([(video vprop) (in-hash track)])
            (define start (video-prop-start vprop))
            (define length (video-prop-length vprop))
            (send dest insert-video (send video copy) i start length)))
        (send dest update-track-pict!)
        (send dest invalidate-bitmap-cache))

      (define/override (write-to-file str)
        ;(super write-to-file str)
        (define (put-exact num)
          (send str put (inexact->exact num)))
        (put-exact track-width)
        (put-exact minimum-width)
        (put-exact frames-per-pixel)
        (put-exact line-frequency)
        (put-exact ruler-height)
        (put-exact track-height)
        (put-exact (hash-count snip-table))
        (put-exact (gvector-count tracks))
        (for ([track (in-gvector tracks)]
              [track# (in-naturals)])
          (for ([(snip vprop) (in-hash track)])
            (put-exact track#)
            (put-exact (video-prop-start vprop))
            (put-exact (video-prop-length vprop))
            (define snipclass (send snip get-snipclass))
            (define classname (send snipclass get-classname))
            (send str put (string->bytes/utf-8 classname))
            (send snip write str)))
        #t)

      (define/override (read-from-file str [overwrite-style #f])
        (let/ec return
          ;(super read-from-file str overwrite-style)
          (set-field! track-width this (send str get-exact))
          (set-field! minimum-width this (send str get-exact))
          (send this set-min-width minimum-width)
          (set-field! frames-per-pixel this (send str get-exact))
          (set-field! line-frequency this (send str get-exact))
          (set-field! ruler-height this (send str get-exact))
          (set-field! track-height this (send str get-exact))
          (define snip-count (send str get-exact))
          (define track-count (send str get-exact))
          (set-tracks! track-count)
          (for ([i (in-range snip-count)])
            (define track# (send str get-exact))
            (define start-time (send str get-exact))
            (define length (send str get-exact))
            (define classname (bytes->string/utf-8 (send str get-bytes)))
            (define snipclass (send (get-the-snip-class-list) find classname))
            (unless snipclass
              (return #f))
            (define snip (send snipclass read str))
            (insert-video snip track# start-time length))
          (update-track-pict!)
          (send this invalidate-bitmap-cache)
          #t))

      ;; Converts the contents of the editor into a video object
      ;; -> Video
      (define/public (read-special source line column position)
        ;; because hygiene is for wimps
        (define data
          `(let ()
             (define multitrack (dynamic-require 'video/base 'multitrack))
             (define blank (dynamic-require 'video/base 'blank))
             (multitrack
              ,@(for/list ([i (in-gvector tracks)])
                  (define track-queue
                    (sort (hash->list i) <
                          #:cache-keys? #t
                          #:key (λ (x)
                                  (define vprop (cdr x))
                                  (video-prop-start vprop))))
                  (define-values (playlist trash)
                    (for/fold ([acc '()]
                               [time 0])
                              ([(snip vprop) (in-dict track-queue)])
                      (define start-delta (- (video-prop-start vprop) time))
                      (define length (video-prop-length vprop))
                      (define acc*
                        (if (zero? start-delta)
                            acc
                            (cons (blank start-delta) acc)))
                      (define snip-stx
                        (send snip read-special source line column position))
                      (values (cons snip-stx acc*) time)))
                  `(list ,@(reverse playlist))))))
        (datum->syntax #f data (list source line column position #f))))))

;; Text Editors can be placed inside of video editors. They are much closer
;;   to the DrRacket text editors.
(define video-text%
  (class* racket:text% (readable<%>)
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
                       (send this insert (make-object video-snip% pb)))])
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
        [_ (super on-event event)]))

    (define/override (copy-self)
      (define vid (new video-text% [track-height track-height]))
      (send this copy-self-to vid)
      vid)

    (define/public (read-special source line column position)
      (define stx (read-syntax source (open-input-text-editor this 0 'end values source #t)))
      (datum->syntax #f
                     `(let () ,stx)
                     (list source line column position #f)))))

;; Video Files are text boxes that represent files.
;; They _can_ be replaced with a video-text
;; with the text (clip <filename>) or (image <filename>)
(define video-file%
  (class* text% (readable<%>)
    (init-field [file #f]
                [track-height 100])
    (super-new)
    (when file
      (send this insert (path->string (file-name-from-path file))))
    (send this lock #t)
    (send this hide-caret #t)

    (define/public (set-file! filename)
      (send this lock #f)
      (send this clear)
      (set! file filename)
      (if (path? filename)
          (send this insert (path->string (file-name-from-path filename)))
          (send this insert "NO FILE"))
      (send this lock #t))

    (define (make-right-click-menu x y)
      (define p (new popup-menu%))
      (new menu-item%
           [parent p]
           [label "Play"]
           [callback (λ (item event)
                       (error "TODO"))])
      (new menu-item%
           [parent p]
           [label "Change File"]
           [callback (λ (item event)
                       (define new-file (get-file))
                       (when new-file
                         (set-file! new-file)))])
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
        [_ (super on-event event)]))
    
    (define/override (copy-self)
      (define vid (new video-file% [file file] [track-height track-height]))
      (copy-self-to vid)
      vid)

    (define/override (copy-self-to other)
      (send other set-file! file))
    
    (define/override (write-to-file str)
      (send str put (if file 1 0))
      (when file
        (send str put (path->bytes file))))
    
    (define/override (read-from-file str [overwrite-style #f])
      (define file? (= (send str get-exact) 1))
      (if file?
          (set-file! (bytes->path (send str get-bytes)))
          (set-file! #f)))
      
    (define/public (read-special source line column position)
      (define stx (read-syntax source (open-input-text-editor this 0 'end values source #t)))
      (define file (symbol->string (syntax-e stx)))
      (define constructor
        (match (path-get-extension file)
          ['png image]
          ['gif image]
          ['jpg image]
          ['mp4 clip]
          [_ clip]))
      (datum->syntax #f
                     `(let () (,constructor ,file))
                     (list source line column position #f)))))

;; Video snips hold video editors.
(define video-snip-class-name
  (~s '((lib "private/editor.rkt" "video")
        (lib "private/editor.rkt" "video"))))

(define video-editor-const 1)
(define video-text-const 2)
(define video-file-const 3)
(define video-snip%
  (class* editor-snip% (readable-snip<%>)
    (init-field [editor #f]
                [with-border? #t]
                [left-margin 5]
                [top-margin 5]
                [right-margin 5]
                [bottom-margin 5]
                [left-inset 1]
                [top-inset 1]
                [right-inset 1]
                [bottom-inset 1]
                [min-width 'none]
                [max-width 'none]
                [min-height 'none]
                [max-height 'none])
    (super-make-object editor
                       with-border?
                       left-margin top-margin right-margin bottom-margin
                       left-inset top-inset right-inset bottom-inset
                       min-width max-width
                       min-height max-height)
    (send this set-snipclass video-snip-class)
    (send (get-the-snip-class-list) add video-snip-class)

    (define/public (read-special source line column position)
      (send editor read-special source line column position))

    (define/override (copy)
      (define new-editor (send editor copy-self))
      (define other
        (make-object video-snip%
             (send editor copy-self)
                   with-border?
                   left-margin top-margin right-margin bottom-margin
                   left-inset top-inset right-inset bottom-inset
                   min-width max-width
                   min-height max-height))
      other)
    
    (define/override (write f)
      (cond
        [(editor . is-a? . video-editor%) (send f put video-editor-const)]
        [(editor . is-a? . video-text%) (send f put video-text-const)]
        [else (send f put video-file-const)])
      (if with-border? (send f put 1) (send f put 0))
      (send f put left-margin)
      (send f put top-margin)
      (send f put right-margin)
      (send f put bottom-margin)
      (send f put left-inset)
      (send f put top-inset)
      (send f put right-inset)
      (send f put bottom-inset)
      (define (maybe-put num)
        (if (eq? num 'none)
            (send f put -1)
            (send f put num)))
      (maybe-put min-width)
      (maybe-put max-width)
      (maybe-put min-height)
      (maybe-put max-height)
      (send editor write-to-file f))))

(define video-snip-class%
  (class snip-class%
    (super-new)
    (send this set-classname video-snip-class-name)
    (define/override (read f)
      (define vid
        (match (send f get-exact)
          [(== video-editor-const) (new video-editor%)]
          [(== video-text-const) (new video-text%)]
          [_ (new video-file%)]))
      (define with-border? (= (send f get-exact) 1))
      (define left-margin (send f get-exact))
      (define top-margin (send f get-exact))
      (define right-margin (send f get-exact))
      (define bottom-margin (send f get-exact))
      (define left-inset (send f get-exact))
      (define top-inset (send f get-exact))
      (define right-inset (send f get-exact))
      (define bottom-inset (send f get-exact))
      (define (maybe-get-exact)
        (define v (send f get-exact))
        (if (negative? v) 'none v))
      (define min-width (maybe-get-exact))
      (define max-width (maybe-get-exact))
      (define min-height (maybe-get-exact))
      (define max-height (maybe-get-exact))
      (send vid read-from-file f)
      (define sn (make-object video-snip%
                   vid
                   with-border?
                   left-margin top-margin right-margin bottom-margin
                   left-inset top-inset right-inset bottom-inset
                   min-width max-width
                   min-height max-height))
      sn)))

(define video-snip-class (new video-snip-class%))

;; A very cludgy hack because we want to use an editor-stream,
;;   but we only get a stream<%>, which has a very similar interface,
;;   but with slightly different names.
;;   This class fixes that.
(define editor-stream-adapter%
  (class object%
    (init-field stream)
    (super-new)
    (define/public (get-exact)
      (send stream read-integer 'video-editor))
    (define/public (read-bytes)
      (send stream read-raw-bytes 'video-editor))))

;; A reader for video snips.
(define video-snip-reader%
  (class* object% (snip-reader<%>)
    (super-new)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (define adapted-stream (make-object editor-stream-adapter% stream))
      (define video-snip (read-snip-from-port video-snip-class-name 'video adapted-stream))
      (cond
        [text-only?
         (define code (send video-snip read-special #f #f #f))
         (string->bytes/utf-8 (~s (syntax->datum code)))]
        [else video-snip]))))

(define video-snip-reader (new video-snip-reader%))
