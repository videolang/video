#lang scribble/manual

@;{
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
}

@require[scribble/core
         scribble/example
         racket/sandbox
         (except-in pict table)
         video/init
         video/private/utils
         video/private/surface
         video/render
         @for-label[(except-in racket/base filter)
                    racket/contract/base
                    racket/set
                    racket/hash
                    (except-in racket/class field)
                    racket/gui/base
                    racket/draw
                    video/init
                    video/base
                    video/core
                    video/render
                    video/player
                    video/init
                    video/lib]]

@title{Rendering}
@defmodule[video/render]

The renderer in this section describes the API for rendering
Video files. A simpler interface is to use the @exec{raco
 video} tool described in @secref["raco-video"].

@section{Render API}

@defproc[(render [data video?]
                 [dest (or/c path path-string? #f) #f]
                 [#:dest-filename dest-filename (or/c path-element? string? #f) #f]
                 [#:render-mixin render-mixin
                  (-> (is-a?/c render<%>) (is-a?/c render<%>))
                  values]
                [#:profile-name profile-name (or/c string? #f) #f]
                [#:width width nonnegative-integer? 720]
                [#:height height nonnegative-integer? 576]
                [#:start start (or/c nonnegative-integer? #f) #f]
                [#:end end (or/c nonnegative-integer? #f) #f]
                [#:fps fps (>=/c 0) 25]
                [#:speed speed real? #f]
                [#:timeout timeout boolean? #f]
                [#:rendering-box rendering-box (or/c box? #f) #f])
         void?]{
                
 Renders a video object.

 @racket[data] is any @racket[video?] object. This can be
 set with either @racket[external-video] or any argument that
 creates a video object, @racket[playlist] or @racket[clip] for example.

 @racket[dest] is the output directory for the resulting file.

 @racket[dest-filename] sets the name of the outputted file
 (if the rendering mixin produces one). By default the
 filename is the same as the input filename with a format appropriate extension.

 The @racket[render-mixin] parameter specifics
 the  output format the renderer producers.
 Many mixins are provided in @secref["render-mixin"]. For
 example, a file can be rendered as an MP4 file using:

 @racketblock[
 (require video/base
          video/render
          video/render/mp4)
 (render (external-video "tomatos.rkt")
         #:render-mixin render-mixin)]

 If @racket[timeout] is specified, the renderer will run for
 the specified time, and will then clean up the file (so it
 can be played in a video player), and quit. This is useful
 for previewing video clips.

 Unless @racket[rendering-box] is @racket[#f], the value in
 the box is filled @racket[rendering-ref?] object. This object is
 only useful for getting the current status of the renderer,
 such as output length, or the current position. For example,
 the progress of a video can be checked with:

 @racketblock[
 (require video/base
          video/render)
 (define rendering-box (box #f))
 (define rendering-thread
   (thread
    (λ ()
      (render (external-video "banana.rkt")
              #:render-box rendering-box))))
 (let loop ()
   (define r (unbox rendering-box))
   (when r
     (define len (get-rendering-length r))
     (define pos (get-rendering-position r))
     (if len
         (printf "\r~a/~a (~a%)~n" pos len (* (/ pos len) 100.0))
         (displayln "Unbounded Video")))
   (sleep 1)
   (when (thread-running? rendering-thread)
     (loop)))]}

@section{Render Class}

The @racket[render%] class interface provides a more
low-level access to Video's rendering interface. Using the
@racket[render] function directly is preferred.

@defclass[render% object% (render<%>)]{
                                   
 The class used for rendering. By default it outputs the
 video directly to the screen. Mixins can be used to extend
 this class to change what it outputs too.
 
 @defconstructor[([dest-dir (or/c path? path-string?)]
                  [dest-filename (or/c path-element? string? #f) #f]
                  [prof-name (or/c string? #f) #f]
                  [width nonnegative-integer? 720]
                  [height nonnegative-integer? 576]
                  [fps (>=/c 0) 25])]{
  Constructs the Renderer with resolution, fps, and
  destination values.}

 @defmethod[(get-fps) (>=/c 0)]
 @defmethod[(get-profile) (or/c profile? #f)]
 @defmethod[(setup-profile) void?]
 @defmethod[(prepare [source video?]) void?]
 @defmethod[(render) void?]
 @defmethod[(play) void?]}

@definterface[render<%> ()]{
 An interface for the @racket[render%] class.
}

@section{Rendering References}

The @racket[render] function uses rendering reference
objects to get real-time information on a video. The object
itself is an opaque box.

@defproc[(rendering-ref? [v any/c]) boolean?]{
 Returns true if @racket[v] is a @racket[rendering-ref?].}

@defproc[(get-rendering-length [ref rendering-ref?]) nonnegative-integer?]{
 Given a @racket[rendering-ref?] object, return the total length of the output in frames.}

@defproc[(get-rendering-position [ref rendering-ref?]) nonnegative-integer?]{
 Given a @racket[rendering-ref?] object, return the renderer's position.}

@section[#:tag "render-mixin"]{Render Mixins}

@subsection{MP4 Rendering}
@defmodule[video/render/mp4]
@defmixin[render-mixin (render<%>) (render<%>)]

@subsection{JPEG Rendering}
@defmodule[video/render/jpg]
@defmixin[render-mixin (render<%>) (render<%>)]

@subsection{PNG Rendering}
@defmodule[video/render/png]
@defmixin[render-mixin (render<%>) (render<%>)]

@subsection{XML Rendering}
@defmodule[video/render/xml]
@defmixin[render-mixin (render<%>) (render<%>)]

@subsection{List Plugins Rendering}
@defmodule[video/render/list]
@defmixin[render-mixin (render<%>) (render<%>)]{
 Rather than outputting any video, this is a special mixin to
 display the plugins installed for MLT. This module may be
 removed in the future.}
