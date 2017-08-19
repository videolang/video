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

@(define vid-eval
   (make-base-eval '(begin (require video/base))))

@title{Getting Started}

Video Language (or just Video) is a functional-declarative language for manipulating
videos from many input sources: images, video files, even
live streams.

The @racketmodname[video] module is a language and includes
all of the bindings in @racketmodname[racket]. Users that
want to use Video as an API embedded in other languages can
use the @racketmodname[video/base] module instead.

@section{Video Basics}

Rather than evaluating programs for their effects,
expressions in Video combine to describe a single video
stream. Thus, Video allows authors to write videos in a
similar fashion to how LaTeX enables authors to write
documents.

All VidLang programs begin with the language declaration @code{#lang video}; and the
remaining program below is a description of the resulting video, written in expressions.

Each top-level expression is a @tech["producer"], which produces a video stream. For example, the
@racket[color] producer generates a stream of green frames:

@racketmod[
 video (code:comment "green.vid")
 (color "green")]

When the module above is converted into a video, the output
looks something like:

@(inset-flow
  (scale (bitmap (build-path video-dir "scribblings" "sample.png")) 0.3))

This picture also shows an example of the playback controls, which
are shown whenever previewing a video. The easiest way
to preview a video is to press the @onscreen{Preview Video}
button (@(bitmap (build-path video-dir "scribblings" "preview.png")))
in the DrRacket toolbar.

Alternatively, the
@exec["raco video"] tool can also preview videos. For
example, say the above video was saved as
@filepath["green.vid"]. Then the preview can be run with:
@;
@nested[#:style 'inset]{@exec["raco video --preview green.vid"]}
@;
Note that simply running a
program is not enough to render a video. Every Video
program describes a single @racket[vid] data structure;
thus, a renderer (or streamer) can prepare the video in many
different formats. Additionally, Video programs can include
the @racket[vid] structure from other programs. Evaluating
@racket[vid] in DrRacket's REPL after running the module
shows the resulting structure:

@examples[;#:eval vid-eval
 (eval:alts vid
            ;(playlist (color "green")))]
            (displayln "#<playlist>"))]

@section{Producers}
A @deftech["producer"] is anything that produces a video stream.

The @racket[color] function creates an infinitely long
@tech["producer"]. The @tech["producer"]'s length---or number of frames---can optionally be set
explicitly with its @tech["properties"]. If the length is
not set, the @tech["producer"]'s length will automatically set itself
to fit the surrounding context. Another function,
@racket[clip], creates a @tech["producer"] from a file:

@racketmod[
 video
 (clip "spinning_square.mp4")]

@inset-flow[
 (apply playlist-timeline the-rr-clip)]

Like @racket[color], clips can also set their length with @tech["properties"]

@racketmod[
 video
 (clip "spinning_square.mp4"
       #:properties (hash "start" 2
                          "end" 8))]

@inset-flow[
 (apply playlist-timeline
        (take (drop the-rr-clip 2) 6))]

@section{Filters}

@deftech["Filters"] can be attached to every producer. These filters
modify the producers behavior: turning it grayscale,
changing the aspect ratio, etc. The @racket[attach-filter]
function attaches filters to an existing producer. For
example, we can use the @racket[grayscale-filter] to remove
the color from the rotating square clip earlier:

@racketmod[
 video
 (attach-filter (clip "spinning_square.mp4")
                (grayscale-filter))]
@inset-flow[
 (apply playlist-timeline (map (compose frame grayscale-pict) the-rr-clip))]

An alternative approach would be to use the
@racket[#:filters] keyword associated with producers.

@racketmod[
 video
 (clip "spinning_square.mp4"
       #:filters (list (grayscale-filter)))]
@inset-flow[(apply playlist-timeline the-grr-clip)]

@section{Playlists}

Video shines when combining multiple producers. The language
provides two ways of combining producers,
@deftech["playlists"] and @tech["multitracks"]. To a first
approximation, @tech["playlists"] run producers
sequentially, while @tech["multitracks"] play them
simultaneously.

Every Video module is implicitly a @tech["playlist"].
Alternatively, @tech["playlists"] can be created with the
@racket[playlist] function.

@racketmod[
 video
 (clip "spinning_square.mp4"
       #:properties (hash "start" 0 "end" 4))
 (clip "spinning_square.mp4"
       #:properties (hash "start" 0 "end" 4)
       #:filters (list (grayscale-filter)))]
@inset-flow[
 (apply playlist-timeline
        (append (slice the-rr-clip 0 4)
                (slice the-grr-clip 4 8)))]
@racketmod[
 video
 (playlist
  (clip "spinning_square.mp4"
        #:properties (hash "start" 0 "end" 4))
  (clip "spinning_square.mp4"
        #:properties (hash "start" 0 "end" 4)
        #:filters (list (grayscale-filter))))]
@inset-flow[
 (apply playlist-timeline
        (append (slice the-rr-clip 0 4)
                (slice the-grr-clip 4 8)))]

@tech["Playlists"] are themselves producers. As such, the
@racket[playlist] function also serves to append multiple playlists
together. This example combines the playlist from above with
another similar clip of a ball dropping:

@racketmod[
 video
 square-movie
 ball-movie
 (define square-movie
   (playlist
    (clip "spinning_square.mp4"
          #:properties (hash "start" 0 "end" 2))
    (clip "spinning_square.mp4"
          #:properties (hash "start" 2 "end" 4)
          #:filters (list (grayscale-filter)))))
 (define ball-movie
   (playlist
    (clip "ball_drop.mp4"
          #:properties (hash "start" 0 "end" 2))
    (clip "ball_drop.mp4"
          #:properties (hash "start" 2 "end" 4)
          #:filters (list (grayscale-filter)))))]
@inset-flow[
 (apply playlist-timeline
        (append (slice the-rr-clip 0 2)
                (slice the-grr-clip 2 4)
                (slice the-ball-drop 0 2)
                (slice the-grall-drop 2 4)))]

This clip also introduces @racket[define] in Video. Unlike
many other @racket[racket]-based languages, module level
variables are defined for the whole module, not just after
their definition.@margin-note{This is also true of functions
 created with @racket[λ/video] and @racket[define/video]. But
 this feature is experimental.}

@section{Transitions}

@deftech["Transitions"] determine how one clip transitions into another
clip in a playlist. For example, a transition can create a
fading or swiping effect from one clip to another.

Transitions can be placed directly in a playlist, and
combine the producers directly before and after them:

@racketmod[
 video
 (clip "ball_drop.mp4"
       #:properties (hash "start" 0 "end" 5))
 (fade-transition 2)
 (clip "ball_drop.mp4"
       #:properties (hash "start" 5 "end" 10)
       #:filters (list (grayscale-filter)))]
@inset-flow[
 (apply playlist-timeline
        (for/list ([c (in-list the-ball-drop)]
                   [g (in-list the-grall-drop)]
                   [i (in-naturals)])
          (cond
            [(< i 3) c]
            [(> i 7) g]
            [else
             (define n (- i 3))
             (cc-superimpose (cellophane c (- 1 (/ n 4)))
                             (cellophane g (/ n 4)))])))]

Alternatively, @tech["transitions"] can be attached with the
@racket[#:transitions] keyword. This parameter takes a list
of @tech["transitions"] that use @racket[#:start] and @racket[#:end]
to specify what producers it connects:

@racketmod[
 video
 (define colored
   (clip "ball_drop.mp4"
         #:properties (hash "start" 0 "end" 5)))
 (define black+white
   (clip "ball_drop.mp4"
         #:properties (hash "start" 5 "end" 10)
         #:filters (list (grayscale-filter))))
 (playlist
  colored
  black+white
  #:transitions (list (fade-transition 2
                                       #:start colored
                                       #:end black+white)))]

@tech["Transitions"] themselves are not @tech["producers"], but
server to combine producers in a @tech["playlist"]. However,
@tech["properties"] can still be attached to a @tech["transition"].

@section{Multitracks and Merges}

@deftech["Multitracks"] play multiple producer
simultaneously. Unlike in a @tech["playlist"], only the top
most track will be rendered. @deftech["Merges"] combine
different tracks in a @racket[multitrack]. These can be
anything from a video overlay, to a chroma key effect. As
with @tech["transitions"] in @tech["playlists"], composite
@tech["merges"] can be inlined with the @tech["producers"]
in the @tech["multitrack"]'s.

@racketmod[
 video
 (multitrack
  (blank #f)
  (composite-merge 0 0 1/2 1)
  (clip "spinning_square.mp4")
  (composite-merge 1/2 0 1/2 1)
  (clip "dropping_ball.mp4"))]
@inset-flow[
 (apply playlist-timeline
        (for/list ([r (in-list the-rr-clip)]
                   [b (in-list the-ball-drop)])
          (shot (hc-append r b))))]

@tech["Merges"] can also be listed separately with the
@racket[#:merges] keyword. This keyword takes a list of
@tech["merges"] that specify their associated tracks with
the @racket[#:top] and @racket[#:bottom] keywords:

@racketmod[
 video
 (multitrack
  bg
  spinning-square
  dropping-ball
  #:merges (list (composite-merge 0 0 1/2 1
                                  #:top spinning-square
                                  #:bottom bg)
                      (composite-merge 1/2 0 1/2 1
                                       #:top dropping-ball
                                       #:bottom bg)))
 (define bg (blank #f))
 (define spinning-square (clip "spinning_square.mp4"))
 (define dropping-ball (clip "dropping_ball.mp4"))]
@inset-flow[
 (apply playlist-timeline
        (for/list ([r (in-list the-rr-clip)]
                   [b (in-list the-ball-drop)])
          (shot (hc-append r b))))]

@section{Video Properties}

Every video object contains a set of run-time
@deftech["properties"], which are a @racket[hash] mapping
@[string]s to any value. They can be set with the
@racket[#:properties] keyword and retrieved with the
@racket[get-property] function:

@racketmod[
 video
 (define the-ultimate-color
   (color "green"
          #:properties (hash "the-ultimate-property" 42)))
 (get-property the-ultimate-color
               "the-ultimate-property") (code:comment "=> 42")]

Some properties are implicitly associated with an object by
the Video runtime. Such as the width and height of an image,
or the length of a clip. For example, the following module
will play the first half of the @filepath{epic.mp4} movie.

@section[#:tag "raco-video"]{Command Line Interaction}

The @exec{raco video} tool is the most simple way to render
video files. To get a list of its current set of features, run:

@nested[@exec{raco video --help}]

By default, @exec{raco video} will render your file to a
pre-specified format. You can also open up a preview window
playing the program with the @DFlag{preview} flag.
