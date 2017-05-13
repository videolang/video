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
                [#:timeout timeout boolean? #f])
         void?]{
 Renders a video object. The given mixin determines what
 output format the renderer producers.}

@section{Render Class}

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

@;{
@definterface[render<%> (get-fps get-profile setup-profile prepare render play)]{
 An interface for the @racket[render%] class.
}}

@section{Render Mixins}

@subsection{JPEG Rendering}
@defmodule[video/render/jpg]
@defthing[render-mixin (-> (is-a?/c render<%>) (is-a?/c render<%>))]

@subsection{PNG Rendering}
@defmodule[video/render/png]
@defthing[render-mixin (-> (is-a?/c render<%>) (is-a?/c render<%>))]

@subsection{XML Rendering}
@defmodule[video/render/xml]
@defthing[render-mixin (-> (is-a?/c render<%>) (is-a?/c render<%>))]

@subsection{List Plugins Rendering}
@defmodule[video/render/list]
@defthing[render-mixin (-> (is-a?/c render<%>) (is-a?/c render<%>))]{
 Rather than outputting any video, this is a special mixin to
 display the plugins installed for MLT. This module may be
 removed in the future.}
