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
                    video/init]]


@title{Video API}

@defmodule[video/base]

Note that not all of the functions in this module are
currently documented.

@section{Bundled Producers}

@defproc[(blank [length (or/c integer? #f)]) producer?]{
 Creates a blank producer of @racket[length] clips.

 If @racket[length] is @racket[#f], then the producer
 generates as many blank frames as its surrounding @racket[multitrack] requires.}

@defproducer[(color [color (or/c string?
                                 (is-a?/c color%)
                                 (list/c byte? byte? byte?))])]{
 Creates a producer that is a solid color.
 @examples[(eval:alts (color "green") (void))
           (eval:alts (color "yellow" #:length 10) (void))
           (eval:alts (color 255 255 0) (void))]}

@defproducer[(image [file (or/c path-string? path?)])]{
 Creates a producer that is a still image.}
              
@defproducer[(clip [file (or/c path-string? path?)]
                   [#:video-index video-index (or/c nonnegative-integer? #f) #f]
                   [#:audio-index audio-index (or/c nonnegative-integer? #f)])]{
 Creates a producer from a video file.}

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

@defproc[(cut-producer [producer producer?]
                       [#:start start (or/c nonnegative-integer? #f) #f]
                       [#:end end (or/c nonnegative-integer? #f) #f])
         producer?]

@section{Bundled Transitions}

@deftransition[(fade-transition)
               #:direction s/e]

@deftransition[(composite-transition [x (or/c (between/c 0 1) pixel?)]
                                     [y (or/c (between/c 0 1) pixel?)]
                                     [width (or/c (between/c 0 1) pixel?)]
                                     [height (or/c (between/c 0 1) pixel?)])
               #:direction t/b]{
                                
 The @racket[x] and @racket[y] coordinates specify the top-left point of
 overlayed image. If a @racket[pixel?] struct is provided
 then the point is in terms of pixels, otherwise the point is
 a number between 0 and 1, 0 being top-left, and 1 being
 bottom-right.

 The @racket[width] and @racket[height] coordinates specify
 the width and height of the overlayed image, either in
 pixels or a ratio.}

@section{Bundled Filters}

@defproc[(grayscale-filter) filter?]

@section{Properties}

Each producer has a table of properties attached to it.
These tables contain both values given to it with the
@racket[#:prop] keyword when the producer is created, and
innate properties based on the producer type. Different
producers will have different types of innate properties,
but some common ones are: @racket["length"],
@racket["width"], and @racket["height"].

@defproc[(get-property [producer video?]
                       [key string?])
         any/c]{
 Gets the attached property associated with the producer.

 If an explicit property was given for @racket[key] to the producer when it
 is created, that is returned first.

 If no explicit property was given for @racket[key], then it
 searches for an innate property.

 If no explicit or innate property is associated with the
 producer, an error is thrown.}

@defproc[(producer-length [producer video?])
         number?]{
 Determines the length of the producer in frames.

 Note that this function will be removed in future versions
 of Video. And will be replaced with @racket[get-property]
 once it has been thoroughly tested.
 
 @racket[producer] is the video who's length will be tested.}

@section{Misc. Functions}

@defform[(external-video module)]{ Given a module path to a
 video file, dynamically require that file, and place its
 @racket[vid] values in place of @racket[external-video].}

@section{Alternate Units}

@defmodule[video/units]

@defstruct[pixel ([value nonnegative-integer?])]
