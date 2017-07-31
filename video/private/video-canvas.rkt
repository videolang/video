#lang at-exp racket/base

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

(provide video-canvas%
         audio-buffer%
         video-canvas-render-mixin)
(require opengl
         opengl/util
         portaudio
         racket/generator
         racket/class
         racket/async-channel
         (except-in racket/gui/base yield)
         racket/format
         racket/match
         ffi/unsafe
         ffi/vector
         "../render.rkt"
         (submod "../render.rkt" render-fields)
         "ffmpeg.rkt"
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

    (field [buff #f]
           [uv-buff #f]
           [tex-buff #f]
           [tex-id #f]
           [prog #f])
    (send this with-gl-context
          (λ ()
            ;; Setup the VAO
            (define arr (u32vector-ref (glGenVertexArrays 1) 0))
            (glBindVertexArray arr)
            ;; Setup the screen coords
            (set! buff (u32vector-ref (glGenBuffers 1) 0))
            (glBindBuffer GL_ARRAY_BUFFER buff)
            (glBufferData GL_ARRAY_BUFFER
                          (* (compiler-sizeof 'float) (f32vector-length vert-coords))
                          vert-coords
                          GL_STATIC_DRAW)
            ;; Setup the UV coords
            (set! uv-buff (u32vector-ref (glGenBuffers 1) 0))
            (glBindBuffer GL_ARRAY_BUFFER uv-buff)
            (glBufferData GL_ARRAY_BUFFER
                          (* (compiler-sizeof 'float) (f32vector-length uv-coords))
                          uv-coords
                          GL_STATIC_DRAW)
            ;; Set up a texture buffer (maybe run later)
            (set! tex-buff (u32vector-ref (glGenTextures 1) 0))
            (glBindTexture GL_TEXTURE_2D tex-buff)
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
            ;; Compile and run the shaders
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
            (glDeleteShader f-shad)
            (glUseProgram prog)
            ;; Setup texture id
            (set! tex-id (glGetUniformLocation prog "myTextureSampler"))
            ;; Finilize steps, maybe remove?
            (glViewport 0 0 width height)
            (glClearColor 0.0 0.0 0.0 0.0)))

    ;; Draw data to a frame. The data-fill-callback fills the
    ;;   opengl buffers. It has no parameters because that is
    ;;   handled (unsafely) by opengl state.
    ;; -> Void
    (define/public (draw-frame data-fill-callback)
      (send this with-gl-context
            (λ ()
              (data-fill-callback)
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D tex-buff)
              (glUniform1i tex-id 0)
              (glEnableVertexAttribArray 0)
              (glBindBuffer GL_ARRAY_BUFFER buff)
              (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
              (glEnableVertexAttribArray 1)
              (glBindBuffer GL_ARRAY_BUFFER uv-buff)
              (glVertexAttribPointer 1 2 GL_FLOAT #f 0 #f)
              (glDrawArrays GL_TRIANGLE_STRIP 0 4)
              (glDisableVertexAttribArray 0)
              (glDisableVertexAttribArray 1)))
      (send this swap-gl-buffers))

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

;; Handles the audio buffer for the video canvas
;; Based on the portaudio library.
(define audio-buffer%
  (class object%
    (super-new)
    (define buff (make-async-channel))
    (define curr-frame #f)
    (define sample-offset 0)
    (define frame-size #f)

    ;; Add a frame to the buffer queue
    (define/public (add-frame frame)
      (async-channel-put buff frame))

    ;; Read the next frame from the queue, discarding
    ;;   the old current-frame if it exists.
    ;; Optionally block until the next frame comes in.
    ;; Only sets the current frame to #f if block is set to #f.
    ;; Boolean -> (void)
    (define/public (read-frame [block #t])
      (when curr-frame
        (av-frame-free curr-frame))
      (set! curr-frame ((if block async-channel-get async-channel-try-get) buff))
      (set! frame-size
            (and curr-frame
                 (av-frame-nb-samples curr-frame)))
      (set! sample-offset 0))

    ;; UNSAFE METHOD
    ;; Feed `count` samples into the `buffer` given.
    ;; This procedure usesee unsafe ffi calls!
    ;; s16vector nonnegative-number -> Void
    (define/public (feed-samples! buffer count [block #t])
      (let/ec return
        (let loop ([pos 0])
          (when (< pos count)
            (when (or (not frame-size)
                      (not sample-offset)
                      (= (- frame-size sample-offset) 0)) ; <-- (= samples-left 0)
              (read-frame block))
            (unless curr-frame
              (return))
            (define samples-left (- frame-size sample-offset))
            (when (= samples-left 0)
              (return))
            (define samples-to-get (min samples-left count))
            ;(displayln samples-left)
            ;(displayln count)
            ;(displayln samples-to-get)
            ;(newline)
            #;
            (memcpy (av-frame-extended-data curr-frame)
                    sample-offset
                    buffer
                    pos
                    (* 2 samples-to-get)
                    _int16)
            (set! sample-offset (+ sample-offset samples-to-get))
            (unless (= samples-to-get count)
              (read-frame block)
              (loop (+ pos samples-to-get)))))
        (void)))))

;; Similar to the Video Renderer, however adds `set-canvas` method for Video
;; to render to a canvas rather than a file.
(define video-canvas-render-mixin
  (mixin (render<%>) ()
    (super-new)
    (inherit-field stop-writing-flag stop-writing-semaphore)
    (define canvas #f)
    (define audio-buffer (new audio-buffer%))
    (define/override (setup rs)
      (super setup (struct-copy render-settings rs
                                [pix-fmt 'rgb24]
                                [sample-fmt 's16]
                                [sample-rate 44100]
                                [format 'raw])))
    (define/public (set-canvas c)
      (set! canvas c))
    (define/override (write-output-callback-constructor #:render-status render-status)
      (λ (mode obj)
        (match obj
          [(struct* codec-obj ([codec-context ctx]
                               [buffer-context buff-ctx]
                               [type type]))
           (match* (type mode)
             [('audio 'open)
              (stream-play/unsafe (λ (buff count)
                                    (send audio-buffer feed-samples! buff count #f))
                                  0.2
                                  44100)]
             [('audio 'write)
              (let loop ()
                (with-handlers ([exn:ffmpeg:again? (λ (e) '())]
                                [exn:ffmpeg:eof? (λ (e) eof)])
                  (define out-frame (av-buffersink-get-frame buff-ctx))
                  (send audio-buffer add-frame out-frame)
                  (loop)))]
             [('video 'write)
              (let loop ()
                (with-handlers ([exn:ffmpeg:again? (λ (e) '())]
                                [exn:ffmpeg:eof? (λ (e) eof)])
                  (define out-frame (av-buffersink-get-frame buff-ctx))
                  (define linesize (array-ref (av-frame-linesize out-frame) 0))
                  (send canvas draw-frame
                        (λ ()
                          (for ([i (in-range (avcodec-context-height ctx))])
                            (glTexSubImage2D
                             GL_TEXTURE_2D
                             0
                             0
                             i
                             (avcodec-context-width ctx)
                             1
                             GL_RGB
                             GL_UNSIGNED_BYTE
                             (ptr-add (array-ref (av-frame-data out-frame) 0)
                                      (* i linesize))))))
                  (av-frame-free out-frame)
                  (when (call-with-semaphore stop-writing-semaphore
                                             (λ () (eq? stop-writing-flag 'running)))
                    (loop))))]
             [(_ _) (void)])])))))
