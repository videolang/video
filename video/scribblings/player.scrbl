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

@title{Player}
@defmodule[video/player]

@defproc[(preview [data producer?])
         (is-a?/c video-player%)]{
                                  
 Helper function to create a new @racket[video-player%]
 object, and start playback. The resulting
 @racket[video-player%] object is returned.
 
 @racket[data] is the @racket[producer] that the video player will render.}

@defclass[video-player% frame% ()]{
                                   
 A video player for previewing producers.
 
 @defconstructor[([video video?])]{
                                   
  Constructs a video player object. This does not
  automatically show the video or start the video playback.
  Use @method[window<%> show] and
  @method[video-player% play], or consider using
  @racket[preview] instead.

  @racket[video] is the initial video associated with the
  player. This can be changed with @method[video-player% set-video].}
 @defmethod[(get-video-length) exact-nonnegative-integer?]
 @defmethod[(play) void?]
 @defmethod[(pause) void?]
 @defmethod[(stop) void?]
 @defmethod[(is-stopped?) boolean?]
 @defmethod[(seek [frame exact-nonnegative-integer?]) void?]
 @defmethod[(set-speed [speed number?]) void?]
 @defmethod[(rewind) void?]
 @defmethod[(fast-forward) void?]
 @defmethod[(get-position) exact-positive-integer?]
 @defmethod[(get-fps) number?]
 @defmethod[(set-video [video producer?]) void?]
}

@defclass[video-player-server% object% ()]{
                                           
 Back-end for the @racket[video-player%] class. Can be used
 to render a video without the surrounding controls.
 
 @defmethod[(set-canvas [c canvas?]) void?]
 @defmethod[(set-video [v producer?]) void?]
 @defmethod[(get-video-length) (and/c real? (>=/c 0))]
 @defmethod[(play) void?]
 @defmethod[(is-paused?) boolean?]
 @defmethod[(get-status) (or/c 'playing
                               'paused
                               'rewinding
                               'fast-forwarding
                               'playing-slow
                               'stopped)]
 @defmethod[(is-stopped?) boolean?]
 @defmethod[(pause) boolean?]
 @defmethod[(stop) boolean?]
 @defmethod[(seek [position (and/c real? (>=/c 0))]) void?]
 @defmethod[(set-speed [speed real?]) void?]
 @defmethod[(rewind) void?]
 @defmethod[(fast-forward) void?]
 @defmethod[(get-fps) (and/c real? (>=/c 0))]
 @defmethod[(render-audio [render? boolean?]) void?]
 @defmethod[(render-video [render? boolean?]) void?]}
