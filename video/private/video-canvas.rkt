#lang at-exp racket/base

#|
   Copyright 2016-2018 Leif Andersen

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

;; The OpenGL use is based off of the OpenGL tutorials found at:
;; http://www.opengl-tutorial.org/

(provide video-canvas%
         audio-buffer%
         video-canvas-render-mixin)
(require opengl
         opengl/util
         portaudio
         racket/generator
         racket/list
         racket/class
         racket/async-channel
         (except-in racket/gui/base yield)
         racket/format
         racket/file
         racket/match
         ffi/unsafe
         ffi/vector
         "log.rkt"
         "../render.rkt"
         (submod "../render.rkt" render-fields)
         "ffmpeg/main.rkt"
         "init.rkt"
         (submod "init.rkt" portaudio)
         (except-in "ffmpeg-pipeline.rkt" render))

;; These fields are private to this module.
(define-local-member-name
  buff
  uv-buff
  tex-buff
  tex-id
  prog)

;; A Video canvas object displays the video data from a file.
;; It is a superclass of canvas% and can thus be placed anywhere a canvas can.
;; Generally it is used with video-canvas-render-mixin%.
(define video-canvas%
  (class canvas%
    (init-field width height)
    (define glconf (new gl-config%))
    (send glconf set-legacy? #f)
    (super-new [gl-config glconf]
               [style '(gl no-autoclear)]
               [min-width width]
               [min-height height]
               [stretchable-width #f]
               [stretchable-height #f])

    (define vert-coords
      (f32vector -1.0 1.0 0.0
                 -1.0 -1.0 0.0
                 1.0 1.0 0.0
                 1.0 -1.0 0.0))
    (define uv-coords
      (f32vector 0.0 0.0
                 0.0 1.0
                 1.0 0.0
                 1.0 1.0))
    (define vert-shader
      @~a{
 #version 330 core
 layout(location = 0) in vec3 vertexPosition_modelspace;
 layout(location = 1) in vec2 vertexUV;
 out vec2 UV;
 void main(){
  gl_Position.xyz = vertexPosition_modelspace;
  gl_Position.w = 1.0;
  UV = vertexUV;
 }})
    (define frag-shader
      @~a{
 #version 330 core
 in vec2 UV;
 out vec3 color;
 uniform sampler2D myTextureSampler;
 void main(){
  color = texture(myTextureSampler, UV).rgb;
 }})
    (define vert-shader/legacy
      @~a{
 #version 120
 attribute vec3 vertexPosition_modelspace;
 attribute vec2 vertexUV;
 varying vec2 UV;
 void main(){
  gl_Position = vec4(vertexPosition_modelspace,1);
  UV = vertexUV;
 }})
    (define frag-shader/legacy
      @~a{
 #version 120
 varying vec2 UV;
 uniform sampler2D myTextureSampler;
 void main(){
  gl_FragColor = texture2D(myTextureSampler, UV);
 }})

    ;; MUST BE CALLED IN AN OPENGL CONTEXT
    ;; Sets this canvas' opengl shaders.
    ;; String String -> Void
    (define (load-program! vert-shader frag-shader)
      (define v-shad (glCreateShader GL_VERTEX_SHADER))
      (define f-shad (glCreateShader GL_FRAGMENT_SHADER))
      (glShaderSource v-shad 1 (vector vert-shader) (s32vector (string-length vert-shader)))
      (glCompileShader v-shad)
      (glShaderSource f-shad 1 (vector frag-shader) (s32vector (string-length frag-shader)))
      (glCompileShader f-shad)
      (set! prog (glCreateProgram))
      (glAttachShader prog v-shad)
      (glAttachShader prog f-shad)
      (glLinkProgram prog)
      (glDetachShader prog v-shad)
      (glDetachShader prog f-shad)
      (glDeleteShader v-shad)
      (glDeleteShader f-shad))

    
    ;; MUST BE CALLED IN AN OPENGL CONTEXT
    ;; Generates a vertex buffer that is filled with the contents of `coords`
    ;; f32vector -> gl-uint
    (define (gen-vertex-buff coords)
      (define buff (u32vector-ref (glGenBuffers 1) 0))
      (glBindBuffer GL_ARRAY_BUFFER buff)
      (glBufferData GL_ARRAY_BUFFER
                    (* (compiler-sizeof 'float) (f32vector-length coords))
                    coords
                    GL_STATIC_DRAW)
      buff)

    ;; MUST BE CALLED IN AN OPENGL CONTEXT
    ;; Generates a texture buffer. Unlike `gen-vertex-buffer`, this buffer
    ;;   is left empty and will be filled by the data filler callback.
    ;; Buffer is allocated to be the same size and width of the video-canvas%
    ;;   class's fields.
    ;; ->  gl-uint
    (define (gen-tex-buff)
      (define buff (u32vector-ref (glGenTextures 1) 0))
      (glBindTexture GL_TEXTURE_2D buff)
      (glTexImage2D GL_TEXTURE_2D
                    0
                    GL_RGB ; Should probably be given to the class
                    width
                    height
                    0
                    GL_RGB ; Should probably be given to the class
                    GL_UNSIGNED_BYTE
                    #f)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      buff)
      
    (define gl-version-type #f)
    (field [buff #f]
           [buff-id #f]
           [uv-buff #f]
           [uv-id #f]
           [tex-buff #f]
           [tex-id #f]
           [prog #f])
    
    (send this with-gl-context
          (λ ()
            ;; First determin if we got the new or old version of opengl.
            ;; Assume the newer version because the `opengl` package provided
            ;; assumes the older version (for error checking).
            (set! gl-version-type
                  (with-handlers ([exn:fail? (λ (e) 'modern)])
                    (if (gl-version-at-least? '(3 3))
                        'modern
                        'legacy)))

            (match gl-version-type
              ['modern
               ;; Setup the VAO
               (define arr (u32vector-ref (glGenVertexArrays 1) 0))
               (glBindVertexArray arr)
               ;; Setup the coords
               (set! buff (gen-vertex-buff vert-coords))
               (set! buff-id 0)
               (set! uv-buff (gen-vertex-buff uv-coords))
               (set! uv-id 1)
               ;; Set up a texture buffer (maybe run later)
               (set! tex-buff (gen-tex-buff))
               ;; Compile and run the shaders
               (load-program! vert-shader frag-shader)
               (glUseProgram prog)
               ;; Setup texture id
               (set! tex-id (glGetUniformLocation prog "myTextureSampler"))]
              ['legacy
               ;; Compile up the program
               ;; (in the legacy config, we compile and then find the attrib locations)
               (load-program! vert-shader/legacy frag-shader/legacy)
               ;; Generate vertex buffers and ids
               (set! buff-id (glGetAttribLocation prog "vertexPosition_modelspace"))
               (set! buff (gen-vertex-buff vert-coords))
               (set! uv-id (glGetAttribLocation prog "vertexUV"))
               (set! uv-buff (gen-vertex-buff uv-coords))
               ;; Generate texture buffers and ids
               (set! tex-id (glGetUniformLocation prog "myTextureSampler"))
               (set! tex-buff (gen-tex-buff))])
            ;; Finilize steps, maybe remove?
            (glViewport 0 0 width height)
            (glClearColor 0.0 0.0 0.0 0.0)))

    ;; Return the video width for the canvas.
    ;; (Note that this can technically be different
    ;;   than the canvas's width.)
    ;; -> Positive-Integer
    (define/public (get-video-width)
      width)
    
    ;; Return the video height for the canvas.
    ;; (Note that this can technically be different
    ;;   than the canvas's height.)
    ;; -> Positive-Integer
    (define/public (get-video-height)
      height)

    ;; Get both the width and height of the video
    ;;   being render in this canvas.
    ;; (Note that this can technically be different
    ;;   than the canvas's width and height.)
    ;; -> (Values Positive-Integer Positive-Integer)
    (define/public (get-dimensions)
      (values (get-video-width) (get-video-height)))

    ;; Draw data to a frame. The data-fill-callback fills the
    ;;   opengl buffers. It has no parameters because that is
    ;;   handled (unsafely) by opengl state.
    ;; -> Void
    (define/public (draw-frame data-fill-callback)
      (define callback-error? #f)
      (with-handlers ([exn:fail? (λ (e)
                                   (when callback-error?
                                     (raise e))
                                   (log-video-warning "OpenGL Error ~a" e)
                                   (void))])
        (send this with-gl-context
              (λ ()
                ;; Fill in data
                (with-handlers ([exn:fail? (λ (e) (set! callback-error? #t) (raise e))])
                  (data-fill-callback))
                ;; Draw in the texture data.
                ;; Unlike the setup, this will be identical for
                ;;   both legacy and modern.
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glUseProgram prog)
                (glActiveTexture GL_TEXTURE0)
                (glBindTexture GL_TEXTURE_2D tex-buff)
                (glUniform1i tex-id 0)
                (glEnableVertexAttribArray buff-id)
                (glBindBuffer GL_ARRAY_BUFFER buff)
                (glVertexAttribPointer buff-id 3 GL_FLOAT #f 0 #f)
                (glEnableVertexAttribArray uv-id)
                (glBindBuffer GL_ARRAY_BUFFER uv-buff)
                (glVertexAttribPointer uv-id 2 GL_FLOAT #f 0 #f)
                (glDrawArrays GL_TRIANGLE_STRIP 0 4)
                (glDisableVertexAttribArray buff-id)
                (glDisableVertexAttribArray uv-id)))
        (send this swap-gl-buffers)))

    (will-register video-canvas%-executor this video-canvas%-final)))

(define (video-canvas%-final this)
  (send this with-gl-context
        (λ ()
          (glDeleteBuffers 3 (u32vector (get-field buff this)
                                        (get-field uv-buff this)
                                        (get-field tex-buff this)))
          (glDeleteTextures 1 (u32vector (get-field tex-id this)))
          (glDeleteProgram (get-field prog this)))))

(define video-canvas%-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute video-canvas%-executor)
      (loop)))))

;; An abstract buffer. Holds the common parts of
;;    video-buffer%'s and audio-buffer%'s implementation.
(define buffer%
  (class object%
    (super-new)
    (field [buff (make-async-channel 50)]
           [curr-frame #f]
           [curr-frame-lock (make-semaphore 1)]
           [time-base #f]
           [start-time (current-inexact-milliseconds)]
           [start-frame #f])

    ;; Set the timebase for the frame's PTSs.
    (define/public (set-time-base tb)
      (set! time-base tb))

    ;; Get the time (in seconds) of the buffer's current position. This
    ;;    does not add or remove anything from the buffer.
    ;; Returns #f if there is no current position (usually because the buffer
    ;;    is not currently playing.
    (define/public (get-current-position)
      (call-with-semaphore
       curr-frame-lock
       (λ ()
         (and curr-frame
              (av-frame-pts curr-frame)))))

    ;; Set the start time. The current time is based on (current-innexact-milliseconds)
    ;; The difference determins what time of the stream should be played
    (define/public (set-start-time [t (current-inexact-milliseconds)])
      (set! start-time t)
      (set! start-frame #f))

    ;; Add a frame to the buffer's internal queue
    (define/public (add-frame frame)
      (async-channel-put buff frame))

    (define/public (read-frame [block #t])
      (error 'buffer "Must be implemented by specific buffer."))

    ;; Flush the buffers (when stream is paused)
    ;; Try-get until #f, which works because the buffer
    ;;   only contains frames.
    (define/pubment (flush-buffer)
      (let loop ()
        (when (send this read-frame #f)
          (loop))))))
  
;; Handles the video buffer for the video canvas.
;; Ensures that they are drawn based on video's timestamps.
;; Uses (current-innexact-milliseconds) to determine the speed that it should
;;    be playing at.
(define video-buffer%
  (class buffer%
    (super-new)
    (init-field canvas)
    (inherit-field buff
                   curr-frame
                   curr-frame-lock
                   start-time
                   start-frame
                   time-base)

    ;; Grab the next frame from the queue
    (define/override (read-frame [block #t])
      (call-with-semaphore
       curr-frame-lock
       (λ ()
         (when curr-frame
           (av-frame-free curr-frame))
         (set! curr-frame ((if block async-channel-get async-channel-try-get) buff))
         (unless curr-frame
           (log-video-debug "VideoBuffer: No frame currently in queue"))
         (not (not curr-frame)))))
    
    (define/public (draw-frame [block #t])
      (unless canvas
        (error 'video-buffer "Must set canvas before calling draw-frame"))
      (let/ec return
        (read-frame block)
        (call-with-semaphore
         curr-frame-lock
         (λ ()
           (unless curr-frame
             (return))
           ;; Wait until to show
           (let loop ()
             (define now (current-inexact-milliseconds))
             (define curr-frame-pts (av-frame-pts curr-frame))
             (unless start-frame
               (set! start-frame curr-frame-pts))
             (define viewer-pos (/ (- now start-time) 1000))
             (define frame-pos (* time-base (- curr-frame-pts
                                               start-frame)))
             (define to-wait (- frame-pos viewer-pos))
             (when (<= 0 to-wait)
               (sleep 0.01)
               (loop)))
           (define linesize (array-ref (av-frame-linesize curr-frame) 0))
           (send canvas draw-frame
                 (λ ()
                   (for ([i (in-range (av-frame-height curr-frame))])
                     (glTexSubImage2D
                      GL_TEXTURE_2D
                      0
                      0
                      i
                      (av-frame-width curr-frame)
                      1
                      GL_RGB
                      GL_UNSIGNED_BYTE
                      (ptr-add (array-ref (av-frame-data curr-frame) 0)
                               (* i linesize))))))))))))

;; Handles the audio buffer for the video canvas
;; Based on the portaudio library.
(define audio-buffer%
  (class buffer%
    (super-new)
    (inherit-field buff curr-frame curr-frame-lock)
    (define sample-offset 0)
    (define frame-size #f)

    ;; Read the next frame from the queue, discarding
    ;;   the old current-frame if it exists.
    ;; Optionally block until the next frame comes in.
    ;; Only sets the current frame to #f if block is set to #f.
    ;; Boolean -> (void)
    (define/override (read-frame [block #t])
      (call-with-semaphore
       curr-frame-lock
       (λ ()
         (when curr-frame
           (av-frame-free curr-frame))
         (set! curr-frame ((if block async-channel-get async-channel-try-get) buff))
         (unless curr-frame
           (log-video-debug "Audiobuffer: No frame currently in queue"))
         (set! frame-size
               (and curr-frame
                    (av-frame-nb-samples curr-frame)))
         (set! sample-offset 0)
         (not (not curr-frame)))))

    ;; UNSAFE METHOD
    ;; Feed `count` samples into the `buffer` given.
    ;; This procedure usesee unsafe ffi calls!
    ;; Only blocks if `block` is true (default)
    ;; Calls pause-callback every frame if provided,
    ;;   callback should return #t if safe to procede
    ;; s16vector nonnegative-number boolean? (or/c #f (-> boolean)
    ;;    -> Void
    (define/public (feed-samples! buffer count [block #t] [pause-callback #f])
      (let/ec return
        (let loop ([pos 0])
          (when (and pause-callback (pause-callback))
            (sleep 0.1)
            (loop pos))
          (define needed-samples (- count pos))
          (define samples-left #f)
          (define samples-to-get #f)
          (unless (<= needed-samples 0)
            (when (or (not frame-size)
                      (not sample-offset)
                      (= (- frame-size sample-offset) 0)) ; <-- (= samples-left 0)
              (read-frame block))
            (call-with-semaphore
             curr-frame-lock
             (λ ()
               (unless curr-frame
                 (memset buffer
                         pos
                         0
                         needed-samples
                         _int32)
                 ;(set! sample-offset #f)
                 (return))
               (set! samples-left (- frame-size sample-offset))
               (set! samples-to-get (min samples-left needed-samples))
               (when (= samples-left 0)
                 (return))
               (memcpy buffer
                       pos
                       (ptr-ref (av-frame-extended-data curr-frame) _pointer 0)
                       sample-offset
                       samples-to-get
                       _int32)
               (set! sample-offset (+ sample-offset samples-to-get))))
               (unless (= samples-to-get needed-samples)
                 (read-frame block)
                 (loop (+ pos samples-to-get)))))
        (void)))))

;; Windows audio devices work better at 48kHz, rather than 44.1kHz.
;; Also portaudio needs to be properly initialized.
(define os-sample-rate
  (case (system-type 'os)
    [(windows) 48000]
    [else 44100]))
(when (eq? (system-type 'os) 'windows)
  (pa-maybe-initialize)
  (host-api 'paWASAPI))

;; Similar to the Video Renderer, however adds `set-canvas` method for Video
;; to render to a canvas rather than a file.
(define video-canvas-render-mixin
  (mixin (render<%>) ()
    (super-new)
    (inherit-field stop-rendering-flag)
    (init [(ic canvas) #f])
    (define canvas #f)
    (define record-primary #f)
    (define play-video? #t)
    (define play-audio? #t)
    (define audio-buffer #f)
    (define video-buffer #f)
    (define stop-audio #f)
    (define video-thread #f)
    (define stop-video-thread-flag #f)
    (define width #f)
    (define height #f)
    (define/public (set-canvas c)
      (set! canvas c)
      (set! width (if c
                      (send c get-video-width)
                      50))
      (set! height (if c
                       (send c get-video-height)
                       50)))
    (set-canvas ic)
    (define (render-settings->live-settings rs)
      (struct-copy render-settings rs
                   [destination (make-temporary-file "out~a.raw")]
                   [pix-fmt 'rgb24]
                   [width width]
                   [height height]
                   [sample-fmt 's16]
                   [sample-rate os-sample-rate]
                   [channel-layout 'stereo]
                   [format 'raw]))
    (define/override (setup rs)
      (unless (and width height)
        (error 'video-canvas-render-mixin
               "Must call set-canvas before setup"))
      (super setup
             (cond [(list? rs)
                    (set! record-primary #t)
                    (list* (first rs)
                           (map render-settings->live-settings (rest rs)))]
                   [else
                    (set! record-primary #f)
                    (render-settings->live-settings rs)])))
    
    (define/override (write-output-callback-constructor #:render-status rs-box
                                                        #:render-tag render-tag)
      (cond
        [(and record-primary (eq? render-tag 'primary))
         (super write-output-callback-constructor
                #:render-status rs-box
                #:render-tag render-tag)]
        [else
         (define (pc) (eq? stop-rendering-flag 'pause))
         (λ (mode obj)
           (match obj
             [(struct* codec-obj ([codec-context ctx]
                                  [buffer-context buff-ctx]
                                  [type type]))
              (define (set-time! type)
                (define buffer (match type
                                 ['video video-buffer]
                                 ['audio audio-buffer]
                                 [_      #f]))
                (when (and buffer rs-box)
                  (define frame (or (send buffer get-current-position)))
                  (when frame
                    (set-render-status-box-position!
                     rs-box (* frame
                               (avcodec-context-time-base ctx))))))
              (set-time! type)
              (match* (type mode)
                [(_ 'init)
                 (set-render-status-box-position! rs-box 0)
                 (set-render-status-box-rendering?! rs-box #t)]
                [('audio 'open)
                 (set! audio-buffer (new audio-buffer%))
                 ;; Since an error is thrown if no audio devices exist,
                 ;; simply turn off audio playing.
                 ;; TODO, should test this _before_ starting to play
                 (when play-audio?
                   (send audio-buffer set-time-base (avcodec-context-time-base ctx))
                   (with-handlers ([exn:fail? (λ (e)
                                                (log-video-error "Caught error: ~a" e)
                                                (set! play-audio? #f))])
                     (match (stream-play/unsafe (λ (buff count)
                                                  (when audio-buffer
                                                    (send audio-buffer feed-samples! buff count #f pc)))
                                                0.1
                                                os-sample-rate)
                       [(list stream-time stats stop)
                        (set! stop-audio stop)])))]
                [('audio 'write)
                 (let loop ()
                   (set-time! 'audio)
                   (with-handlers ([exn:ffmpeg:again? (λ (e) '())]
                                   [exn:ffmpeg:eof? (λ (e) eof)])
                     (define out-frame (av-buffersink-get-frame buff-ctx))
                     (if play-audio?
                         (send audio-buffer add-frame out-frame)
                         (av-frame-free out-frame))
                     (if (eq? stop-rendering-flag #t)
                         eof
                         (loop))))]
                [('audio 'close)
                 (send audio-buffer flush-buffer)
                 (set! audio-buffer #f)
                 (when (and play-audio? stop-audio)
                   (stop-audio)
                   (set! stop-audio #f))]
                [('video 'open)
                 (unless canvas
                   (log-video-warning "Player: Canvas must be set to play video"))
                 (when canvas
                   (set! video-buffer (new video-buffer% [canvas canvas]))
                   (send video-buffer set-time-base (avcodec-context-time-base ctx))
                   (set! stop-video-thread-flag #f)
                   (set! video-thread
                         (thread
                          (λ ()
                            (let loop ()
                              (unless stop-video-thread-flag
                                (send video-buffer draw-frame #f)
                                (sleep 0.01)
                                (loop)))))))]
                [('video 'write)
                 (let loop ()
                   (set-time! 'video)
                   (with-handlers ([exn:ffmpeg:again? (λ (e) '())]
                                   [exn:ffmpeg:eof? (λ (e) eof)])
                     (define out-frame (av-buffersink-get-frame buff-ctx))
                     (if (and play-video? video-buffer)
                         (send video-buffer add-frame out-frame)
                         (av-frame-free out-frame))
                     (if (eq? stop-rendering-flag #t)
                         eof
                         (loop))))]
                [('video 'close)
                 (when video-buffer
                   (set! stop-video-thread-flag #t)
                   (and video-thread (thread-wait video-thread))
                   (when play-video?
                     (send video-buffer flush-buffer)))
                 (set! video-buffer #f)
                 (set-render-status-box-rendering?! rs-box #f)]
                [(_ _) (void)])]))]))))
