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

@title{The Video Language Guide}
@author{Leif Andersen}

@(defmodulelang video)

@(define (colorize #:color c . content)
   (elem #:style (style #f (list (color-property c)))
         content))

@margin-note{Video is still an experimental language. As of
 v0.2, the underlying library and surface syntax are stable.
 New functions may be removed or added, but these changes
 will be documented, and be marked as deprecated before
 removal. The graphical development environment extension is
 still under active development and is subject to change.}

Video Language (or VidLang, sometimes referred to as just Video)
is a DSL for editing...videos. It aims to merge the
capabilities of a traditional graphical non-linear video
editor (NLVE), with the power of a programming language. The
current interface is similar to that of HTML, LaTeX, or
Scribble. VidLang comes with a prototype graphical NLVE for
DrRacket. This extensions is still experimental and highly
unstable. The next version of Video will focus on this
extension and make the tool significantly cleaner and stable.

In case you found these docs from the Racket website, the
Video website can be found at @url{http://lang.video}. It includes
the Video blog, community projects, tutorials, etc.

@table-of-contents[]

@include-section{intro.scrbl}
@include-section{api.scrbl}
@include-section{player.scrbl}
@include-section{render.scrbl}
         
@section{Graphical Non-Linear Video Editors}

The graphical NLVE is very experimental and will likely
break with newer versions of the library.

You can test drive it by going to
@menuitem["Insert" "Insert Video Editor"]

@section{Core Library}
@defmodule[video/core]

The structs given to the @racket[render] function are found
here.

@deprecated-text{Note that these structs are not considered
 stable. And may change between minor versions. (They should
 be stable in point releases.)}

@defstruct*[video ([mlt-object any/c] [tags (set/c any/c)])]{
 The basic video object.

 @racket[mlt-object] contains the compiled mlt-object, or
 @racket[#f] if it has not yet been compiled. This bit of the
 library is particularly unstable, and may change if it stops
 using MLT as a backend.}

@defstruct*[(link video) ([source video?] [target video?] [index exact-nonnegative-integer?])]{
 A link object which connects a producer to a consumer. Links
 can also connect to and from filters. The most basic and
 common connection connects a video producer, to a built in
 consumer. The @racket[preview] and @racket[render] functions
 handle the linking for you.

 As such, this is only required if you plan to use
 @racket[convert-to-mlt!] directly.

 @racket[source] contains the producer that is linked together.
 
 @racket[target] contains the consumer that receives the
 produced video.}

@defstruct*[(properties video) ([prop (hash/c string? any/c)])]{
 Contains properties attached to video objects. These are
 currently only used as to interact with the MLT layer. Such
 as to set a consumers resolution, or set a producers length.

 @racket[prop] contains the actual properties table.}

@defproc[(properties-ref [dict properties?]
                         [key string?]
                         [default-type (or/c 'string 'int 'int64 'mlt-position 'double)])
         any/c]{

 A function to get the properties out of a video object. If
 the property associated with @racket[key] does not exist in
 the table, it will search the MLT object's properties for
 the given @racket[key] if it has already been compiled.

 @racket[dict] is the video object to search.

 The @racket[key] identifies which property to search for.

 @racket[default-type] indicates the assumed type used when
 requesting the object from MLT. This is required because MLT
 does not store this information and internally represents
 everything as a string.}

@defstruct*[(anim-property video) ([value any/c] [position number?] [length number?])]{
 Used for anim-properties in MLT.

 @racket[value] is the actual property.

 @racket[position] and @racket[length] are extra field when setting the anim property.
}

@defstruct*[(frame properties) ()]{
 A struct containing a frame object. This currently has no
 additional data apart from properties. }

@defstruct*[(service properties) ([filters (listof filter)])]{
                                                              
 A service is the basic type of video object that actually handles data. Services come in four types:

 @itemlist[
 @item{@racket[producer] --- Producers actually create
   videos. A producer can be anything from a picture, to a
   video file, to text.}
 @item{@racket[consumer] --- Consumers consume the content
   from a producer (or filter), and stream it to a source that
   accepts videos. An example consumer can output the video to
   a file.}
 @item{@racket[transition] --- A transition combines two
   videos together, such as to fade two clips. These are
   generally used inside of @racket[playlist]s.}
 @item{@racket[filter] --- Filters modify an aspect of the
   video, such as to make the video grayscale.}]

 A service can additionally have filters attached to it, stored in @racket[filters].}

@defstruct*[(filter service) ([type (or/c symbol? #f)] [source (or/c string? #f)])]{
 Filters alter the behavior of a @racket[service] in some
 way. They can be used in one of three ways:
 
 @itemlist[
 @item{Filters can be attached to services directly in the
   @racket[filters] field for that service.}
 @item{Filters can be linked in between a producer and a
   consumer.}
 @item{Filters can be added to a @racket[field] that is associated with a @racket[multitrack].}]

 Many @racket[filter]s apply to only one @racket[producer].
 For example, the grayscale filter turns a producer to grayscale.

 Other filters, however, become relevant only when multiple
 tracks are used in a @racket[tractor]. For example, a filter
 can be used to shrink a video and overlay it on top of
 another one.
 
 @racket[type] stores the type of filter. This presumes that
 the needed plugin is installed for MLT. @racket[#f] is the
 default type, which is generally @racket['loader].

 @racket[source] is the source for the filter. This can be
 used to set some default properties that MLT uses when setting default values.}

@defstruct*[(transition service) ([type (or/c symbol? #f)]
                                  [source (or/c string? #f)]
                                  [length integer?])]{
                                                      
 Transitions are used for combining two videos, generally
 used in conjunction with a @racket[playlist].

 @racket[type] indicates the type of the transition.

 @racket[source] indicates the source for the transition if
 the type requires it. This generally remains @racket[#f].

 @racket[length] indicates the length of the transition.
 Generally the @racket[playlist] associated with this
 transition will be shorted by @racket[length] frames.}

@defstruct*[(consumer service) ([type (or/c symbol? #f)] [target (or/c string? #f)])]{
 Consumers output a video that is produced by the @racket[producer] that they are linked to.
                                                  
 @racket[type] stores what the consumer outputs. Some common types used are:

 @itemlist[
 @item{@racket['avformat] --- Used for outputting videos
   using @tt{FFMPEG}.}
 @item{@racket['sdl] --- Used for previewing videos an SDL
   window.}
 @item{@racket['xml] --- Used for producing an XML file that
   the @exec{melt} command line tool can process.}]

 The @racket[target] field indicates the file to create, or
 a different parameter that the specific plugin uses. It is
 frequently @racket[#f] for the default consumer.}

@defstruct*[(producer service) ([type (or/c symbol? #f)]
                                [source (or/c string? #f)]
                                [start exact-nonnegative-integer?]
                                [end exact-nonnegative-integer?]
                                [speed number?]
                                [seek exact-nonnegative-integer?])]{
                                                                    
 A @racket[producer] feeds data to consumers or filters. A
 producer can be a video, or an image, or an audio clip.
 
 @racket[type] stores the type of video being inputted.
 Unlike with consumers, this should typically be left as the
 default @racket[#f] or @racket['loader].@margin-note{Yes,
  yes, I know, this is really kludgy. It is currently this way
  to deal with @tt{MLT} bugs.}

 @racket[source] is a URI for producer's source. Some example values are:
 @itemlist[
 @item{"color:blue"}
 @item{"hold:picture.png"}
 @item{"video.mp4"}
 @item{"xml:mltxml.xml}]

 The @racket[start] and @racket[end] of the producer is
 given, or is the length of the producer when it is
 @racket[#f]. Note that some producers, such as still images,
 do not have a natural start or end, and will fill up as much
 time as needed.

 The @racket[speed] of the producer sets the speed the
 producer feeds frames into its consumer. A speed of
 @racket[1] is regular playing speed. @racket[0] is paused
 and will continuously output the same frame. Values greater
 than @racket[1] play faster than the producers speed, and
 values less than @racket[0] rewind the producer. Finally,
 values between @racket[0] and @racket[1] play at a slower
 than normal rate.

 @racket[seek] can be combined with @racket[speed] to alter
 the way the video is played. @racket[seek] determines the
 point where the producer begins to play. Normally this is
 the same position as @racket[start], but could be set to the
 middle of the clip and @racket[speed] set to a negative
 value, to simulate a rewinding clip.}

@defstruct*[(playlist producer)
            ([elements (listof (or/c producer transition blank playlist-producer))])]{
                                                                                                   
 A playlist is a list of producers that play one after the
 other.

 Transitions can also be put in a playlist and are used to
 combine two clips, such as to fade from one video to
 another. A transition shortens its two surrounding clips and
 producers a new clip for the transition. Note that a
 playlist with transitions in it will be shortened by the
 length of the transition.

 @racket[elements] is the list of producers and transitions in the video.}

@defstruct*[(playlist-producer video) ([producer producer?]
                                       [start exact-nonnegative-integer?]
                                       [end exact-nonnegative-integer?])]{
 Don't use this for now. I'm still not entirely sure how its
 supposed to work.}

@defstruct*[(blank video) ([length exact-nonnegative-integer?])]{
 Similar to a producer, but is blank. These can be inserted into playlists.}

@defstruct*[(multitrack producer) ([tracks (listof producer)]
                                   [field (listof field-element?)])]
@defstruct*[(field-element video) ([element (or/c transition? filter? #f)]
                                   [track (or/c exact-nonnegative-integer? #f)]
                                   [track-2 (or/c exact-nonnegative-integer? #f)])]

@section{Extending Video}

@defmodule[video/surface]

Video also has basic syntax for creating new types of
producers, transitions, and filters.

@colorize[#:color "red"]{
 This API is very experimental and WILL change without
 notice. Proceed with caution. The documentation only lists
 the forms available, as the details themselves will change.}


@defidentifier[#'define-producer]
@defidentifier[#'->producer]
@defidentifier[#'defproducer]
@defidentifier[#'define-transition]
@defidentifier[#'->transition]
@defidentifier[#'deftransition]

@index-section[]
