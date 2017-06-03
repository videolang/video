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

(provide video-canvas%)
(require opengl
         opengl/util
         racket/class
         racket/gui/base
         racket/format
         ffi/unsafe
         ffi/vector)

(define video-canvas%
  (class canvas%
    (init-field width height)
    (define glconf (new gl-config%))
    (send glconf set-legacy? #f)
    (super-new [gl-config glconf]
               [style '(gl no-autoclear)])

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

    (define buff #f)
    (define uv-buff #f)
    (define tex-buff #f)
    (define tex-id #f)
    (define prog #f)
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

    ;; XXX NEEDS TO FINALIZE BUFFERS BEFORE GC!!!
    ))
