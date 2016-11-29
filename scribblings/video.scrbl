#lang scribble/manual
@require[scribble/core
         scribble/example
         @for-label[(except-in racket/base filter)
                    racket/contract/base
                    racket/set
                    racket/hash
                    (except-in racket/class field)
                    racket/gui/base
                    video/core
                    video/render
                    video/player
                    video/init
                    video/tag
                    video/lib]]

@title{Video Language}
@author{Leif Andersen}

@(define (colorize #:color c . content)
   (elem #:style (style #f (list (color-property c)))
         content))

@colorize[#:color "red"]{
  This is a highly unstable and experimental DSL for editing videos. Do
  not use this library in production as core parts of it are still being
  designed.}

Video Language is a DSL for
editing...videos.@margin-note["Creative name, I know..."]
Designed with similar designs in mind to Scribble,
Slideshow, and Pict. It is still a work in progress and the
interface may or may not change in the near future.

This libraries requires that you have
@hyperlink["https://mltframework.org/"]{libmlt} installed on your
system.

@margin-note{Many of the implementation decisions of this
 library are based on my understanding of
 @hyperlink["https://mltframework.org/"]{libmlt}, and even
 requires the library to be installed on your machine to
 work. Unfortunately, libmlt lacks comprehensive
 documentation. If you see any invalid uses of libmlt, please
 notify me or submit a patch.}

@table-of-contents[]

@section{Player}
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
         
@section{Rendering}
@defmodule[video/render]

@defproc[(render [data video?])
         void?]{
 Renders a video object using @tt{MLT}. Examples
 of how to use it in @tt{private/examples.rkt}.}

@defproc[(convert-to-mlt! [data video?] [profile any/c #f]) any/c]{
 Compiles a video object to a @tt{MLT} format. The resulting
 compiled video object is also stored in the original data
 object passed in. If the object has already been compiled,
 nothing will occur.

 This method is useful if you wish to modify the traversal
 of compilation. Such as, if you want to compile the second
 producer in a playlist before you compile the first.

 Because this additionally modifies the input, it can still be passed into @racket[render].

 @racket[data] is the video to be compiled. If it already
 has been converted this function does nothing.

 @racket[profile] is an @tt{MLT} profile object. For now
 leave it as @racket[#f] and pretend it doesn't exist.}

@section{Core Library}
@defmodule[video/core]

The structs given to the @racket[render] function are found
here. Examples are found in @tt{private/examples.rkt}.

@defstruct*[video ([mlt-object any/c] [tags (set/c any/c)])]{
 The basic video object.

 @racket[mlt-object] contains the compiled mlt-object, or
 @racket[#f] if it has not yet been compiled. This bit of the
 library is particularly unstable, and may change if it stops
 using MLT as a backend.

 The @racket[tags] field is used for other objects to find this object using @racket[find-tag],
 similarly to the @racketmodname[ppict/tag] library.}

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
                                [speed integer?]
                                [seek exact-nonnegative-integer?])]
@defstruct*[(playlist producer) ([elements (listof producer)])]
@defstruct*[(playlist-producer video) ([producer producer?]
                                       [start exact-nonnegative-integer?]
                                       [end exact-nonnegative-integer?])]
@defstruct*[(blank video) ([length exact-nonnegative-integer?])]
@defstruct*[(multitrack producer) ([tracks (listof producer)]
                                   [field (listof field-element?)])]
@defstruct*[(field-element video) ([element (or/c transition? filter? #f)]
                                   [track (or/c exact-nonnegative-integer? #f)]
                                   [track-2 (or/c exact-nonnegative-integer? #f)])]

@section{Tagging Videos}
@defmodule[video/tag]
@defproc[(tag-video [video video?] [tag any/c])
         video?]

@defproc[(find-tag [video video?] [tag any/c])
         video?]

@section{Video Lib}
@defmodule[video/lib]

@defproc[(converted-video? [video video?]) boolean?]{
                                                     
 Determines if the video has been compiled (using @racket[convert-to-mlt!] or @racket[render]).

 @racket[video] is the video to test.}

@defproc[(producer-length [producer (and/c video? converted-video?)])
         number?]{
 Determines the length of the producer in frames.

 Note that this function requires the video to already be
 converted. In the future, this restriction may be lifted.
 Also note that this function uses @tt{MLT}'s length feature, which
 seems to be a bit buggy at times.

 @racket[producer] is the video who's length will be tested.}

@defproc[(producer-length/uneditied [producer (and/c video? converted-video?)])
         number?]
@defproc[(playlist-clip-length [playlist playlist?] [index exact-nonnegative-integer?])
         exact-nonnegative-integer?]
@defproc[(playlist-clip-start [playlist playlist?] [index exact-nonnegative-integer?])
         exact-nonnegative-integer?]

@defproc[(video-type [video video?]) identifier?]{
                                                  
 Returns an identifier for the type of video as defined in
 @racket[video/core]. @colorize[#:color "red"]{This function
  in particular is likely to get removed!!!}}
