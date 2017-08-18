#lang reader "viddoclang.rkt"

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

@title{Video API}

@defmodule[video/base]

Functions that are deprecated and will be removed or altered
in a backwards compatible breaking way are marked as
@deprecated-text["DEPRECATED"] in red.

@section{Bundled Producers}

@defproducer[(blank [length (or/c integer? #f) #f])]{
 Creates a blank producer of @racket[length] clips.

 If @racket[length] is @racket[#f], then the producer
 generates as many blank frames as its surrounding @racket[multitrack] requires.

 @examples[#:eval video-evaluator
           (blank)
           (blank 10)]}

@defproducer[(color [color (or/c string?
                                 (is-a?/c color%)
                                 (list/c byte? byte? byte?)
                                 byte?)]
                    [c2 (or/c byte? #f) #f]
                    [c3 (or/c byte? #f) #f])]{
 Creates a producer that is a solid color.

 The given color can be a string from
 @racket[color-database<%>], a @racket[color%] object, a list
 of three bytes, or thee seperate bytes.

 If a byte is given for @racket[color], then @racket[c2] and
 @racket[c3] must also contain a byte. Otherwise @racket[c2]
 and @racket[c3] must be @racket[#f].
 
 @examples[#:eval video-evaluator
           (color "green")
           (color "yellow" #:properties (hash "length" 10))
           (color 255 255 0)]}

@defproducer[(clip [file (or/c path-string? path?)])]{
 Creates a producer from a video or image file.

 @examples[#:eval video-evaluator
           (clip "groovy.mp4")
           (clip "fancy.png")]}

@section{Video Compositing}

@defproc[(playlist [producer (or/c producer? transition?)] ...
                   [#:transitions transitions (listof field-element?) '()]
                   [#:properties properties (dictof string? any/c) (hash)])
         producer?]{
                    
 Creates a @tech["playlist"] out of the given
 @tech["producers"]. The first and last element of the list
 must be @tech["producers"]. Additionally, no two @tech["transitions"] can
 appear without a producer in between them.

 @examples[#:eval video-evaluator
           (playlist)
           (playlist (color "blue"))
           (playlist (color "green" #:properties (hash "length" 10))
                     (fade-transition 42)
                     (clip "movie.mp4"))]}

@defproc[(multitrack [producer (or/c producer? merge?)] ...
                     [#:transitions transitions (listof field-element?) '()]
                     [#:properties properties (dictof string? any/c) (hash)])
         producer?]{

 Creates a @tech["multitrack"]. This form is syntactically
 similar to @racket[playlist], but the result renders clips
 in parallel rather than sequentially. Additionally,
 @tech["multitracks"] contain @tech["merges"] instead of
 @tech["transitions"].

 As with @racket[playlist], the first and last elements must
 be @tech["producers"], and no two @tech["merges"] can appear
 without a @tech["producer"] between them.

 @examples[#:eval video-evaluator
           (multitrack)
           (multitrack (clip "hyper.mp4"))
           (multitrack (color "black")
                       (overlay-merge 10 10 50 50)
                       (clip "space.mp4"))]}

@defproc[(attach-filter [producer producer?]
                        [filter filter?] ...)
         producer?]

@defproc[(cut-producer [producer producer?]
                       [#:start start (or/c nonnegative-integer? #f) #f]
                       [#:end end (or/c nonnegative-integer? #f) #f])
         producer?]

@section{Bundled Transitions}

@deftransition[(fade-transition)]

@defmerge[(composite-merge [x (or/c (between/c 0 1) pixel?)]
                           [y (or/c (between/c 0 1) pixel?)]
                           [width (or/c (between/c 0 1) pixel?)]
                           [height (or/c (between/c 0 1) pixel?)])]{
                                
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
