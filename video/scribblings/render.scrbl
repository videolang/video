#lang reader video/scribblings/viddoclang

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

@title{Rendering}
@defmodule[video/render]

The renderer in this section describes the API for rendering
Video files. A simpler interface is to use the @exec{raco
 video} tool described in @secref["raco-video"].

@section{Render API}

@defproc[(render [data video?]
                 [dest (or/c path path-string? #f) "out.mp4"]
                 [#:dest-filename dest-filename (or/c path-element? string? #f) #f]
                 [#:render-mixin render-mixin
                  (-> (is-a?/c render<%>) (is-a?/c render<%>))
                  values]
                [#:width width nonnegative-integer? 1920]
                [#:height height nonnegative-integer? 1080]
                [#:start start (or/c nonnegative-integer? #f) #f]
                [#:end end (or/c nonnegative-integer? #f) #f]
                [#:fps fps (>=/c 0) 25])
         void?]{
                
 Renders a video object. This function is completely
 synchronous and it is impossible to determine the progress
 within Racket. If being asynchronous or just determining
 progress is important, use @racket[render/async].

 @racket[data] is any @racket[video?] object. This can be
 set with either @racket[external-video] or any argument that
 creates a video object, @racket[playlist] or @racket[clip] for example.

 @racket[dest] is the output file. If the path is relative
 it is resolved against @racket[current-directory]. The
 output format can be auto-detected by the extension of the
 @racket[dest] file. For example, the renderer will produce
 an @tt{MP4} file (with appropriate codecs) for a file ending
 in @filepath{.mp4}.

 The @racket[render-mixin] parameter specifics an optional
 mixin for the renderer. This is useful for altering the
 behavior of the renderer, such as to render to an OpenGL
 context.

 The @racket[start], @racket[end], @racket[width], and
 @racket[height] parameters are all optional, and set their
 relative properties in resulting output. Additional options
 exist when using the @racket[render%] class directly.}

@defproc[(render/async [data video?]
                       [dest (or/c path path-string? #f) "out.mp4"]
                       [#:dest-filename dest-filename (or/c path-element? string? #f) #f]
                       [#:render-mixin render-mixin
                        (-> (is-a?/c render<%>) (is-a?/c render<%>))
                        values]
                       [#:width width nonnegative-integer? 1920]
                       [#:height height nonnegative-integer? 1080]
                       [#:start start (or/c nonnegative-integer? #f) #f]
                       [#:end end (or/c nonnegative-integer? #f) #f]
                       [#:fps fps (>=/c 0) 25]
                       [#:mode mode (or/c 'verbose #f) #f])
         (values async-channel? (-> void?))]{
                                             
 Similar to @racket[render], but runs asynchronously. This
 is useful for determining the status while still rendering.

 When called, the @racket[render/aync] function begins
 rendering and immediately returns an output channel. While
 rendering, it places its current status on the channel. The
 renderer puts @racket[eof] on the channel to signal that it
 has finished and will send no more messages.

 If the optional @racket[mode] flag is set to
 @racket['verbose], then the renderer will additionally
 return status messages.}

@defproc[(render/pretty [data video?]
                        [dest (or/c path path-string? #f) "out.mp4"]
                        [#:dest-filename dest-filename (or/c path-element? string? #f) #f]
                        [#:render-mixin render-mixin
                         (-> (is-a?/c render<%>) (is-a?/c render<%>))
                         values]
                        [#:width width nonnegative-integer? 1920]
                        [#:height height nonnegative-integer? 1080]
                        [#:start start (or/c nonnegative-integer? #f) #f]
                        [#:end end (or/c nonnegative-integer? #f) #f]
                        [#:fps fps (>=/c 0) 25]
                        [#:port port (or/c output-port? #f) #f]
                        [#:mode mode (or/c 'verbose #f)])
         void?]{
                
 Similar to @racket[render/async], but will run
 synchronously. Additionally, rather than making a channel
 and sending data to that channel, the result will be printed to @racket[current-output-port].

 If @racket[port] is provided, then the output will be sent
 to that port rather than #racket[current-output-port].

 This function is designed to be used directly when repl-like interaction is desired.}

@section{Render Class}

The @racket[render%] class interface provides a more
low-level access to Video's rendering interface. Using the
@racket[render] function directly is preferred.

@deprecated-text{Unlike the previous section, the
 @racket[render%] class is unstable. It should remain stable
 between patch releases, but can change between minor releases.}

@defclass[render% object% (render<%>)]{
                                   
 The class used for rendering. By default it outputs the
 video directly to the screen. Mixins can be used to extend
 this class to change what it outputs too.

 
 
 @defconstructor[([source video?])]{
                                    
  Constructs the Renderer for the given video
  @racket[source]. The renderer can process the video multiple
  times, but setup must be called in between uses.}

 @defmethod[(setup [settings render-settings?]) void?]{

  Sets up the render properties (width, height, etc.). Must
  be called between each use of @racket[start-rendering].}
 
 @defmethod[(start-rendering) void?]
 @defmethod[(stop-rendering) void?]}

@definterface[render<%> ()]{
 An interface for the @racket[render%] class.
}

@section{Render Settings}

The @racket[render-settings] struct determines the available settings when rendering. 

@defproc[(render-settings? [settings any/c]) boolean?]{

 Returns @racket[#t] if @racket[settings] is a
 @racket[render-settings?], @racket[#f] otherwise.}

@defproc[(make-render-settings [#:destination dest (or/c path? path-string? #f) #f]
                               [#:width width (and/c integer? positive?) 1920]
                               [#:height height (and/c integer? positive?) 1080]
                               [#:start start (or/c (and/c real? (>=/c 0)) #f) #f]
                               [#:end end (or/c (and/c real? (>=/c 0)) #f) #f]
                               [#:fps fps real? 30]
                               [#:format format (or/c symbol? #f) #f]
                               [#:video-codec video-codec (or/c symbol? #f) #f]
                               [#:audio-codec audio-codec (or/c symbol? #f) #f]
                               [#:subtitle-codec subtitle-codec (or/c symbol? #f) #f]
                               [#:pix-fmt pix-fmt symbol? 'yuv420p]
                               [#:sample-fmt sample-fmt symbol? 'fltp]
                               [#:sample-rate sample-rate (and/c real? positive?) 44100]
                               [#:channel-layout channel-layout symbol? 'stereo]
                               [#:speed speed real? 1])
         render-settings?]{
                           
 Constructs a @racket[render-settings?] object. Every field is optional.
}
