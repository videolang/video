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
         video/private/utils
         video/private/surface
         @for-label[(except-in racket/base filter)
                    racket/contract/base
                    racket/set
                    racket/hash
                    (except-in racket/class field)
                    racket/gui/base
                    racket/draw
                    video/base
                    video/core
                    video/render
                    video/player
                    video/init
                    video/lib]]


@title{Video API}

@defmodule[video/base]

Note that not all of the functions in this module are
currently documented.

@section{Bundled Producers}

@defproducer[(color [color (or/c string?
                                 (is-a?/c color%)
                                 (list/c byte? byte? byte?))])]{
 Creates a producer that is a solid color.
 @examples[(eval:alts (color "green") (void))
           (eval:alts (color "yellow" #:length 10) (void))
           (eval:alts (color 255 255 0) (void))]}

@defproducer[(image [file (or/c path-string? path?)])]{
 Contents}
              
@defproducer[(clip [file (or/c path-string? path?)])]{
 Stuff}

@section{Video Compositing}

@defproc[(playlist [producer producer?] ...
                   [#:transitions transitions (listof field-element?) '()]
                   [#:start start (or/c nonnegative-integer? #f) #f]
                   [#:end end (or/c nonnegative-integer? #f) #f]
                   [#:length length (or/c nonnegative-integer? #f) #f])
         producer?]

@defproc[(multitrack [producer producer?] ...
                     [#:transitions transitions (listof field-element?) '()]
                     [#:start start (or/c nonnegative-integer? #f) #f]
                     [#:end end (or/c nonnegative-integer? #f) #f]
                     [#:length length (or/c nonnegative-integer? #f) #f])
         producer?]

@defproc[(attach-filter [producer producer?]
                        [filter filter?] ...)
         producer?]

@section{Bundled Transitions}

@deftransition[(fade-transition)
               #:direction s/e]

@section{Bundled Filters}

@defproc[(grayscale-filter) filter?]

@section{Properties}

@defproc[(producer-length [producer (and/c video? converted-video?)])
         number?]{
 Determines the length of the producer in frames.

 Note that this function requires the video to already be
 converted. In the future, this restriction may be lifted.
 Also note that this function uses @tt{MLT}'s length feature, which
 seems to be a bit buggy at times.

 @racket[producer] is the video who's length will be tested.}
