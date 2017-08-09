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

@require[pict
         video/private/utils
         scribble/example
         racket/sandbox
         racket/list
         racket/math
         "utils.rkt"
         @for-label[video
                    video/base]]

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

All VidLang programs begin with @code{#lang video}, the
remaining program is a description of the resulting video.
Each top level expression is a @tt{producer} which is
anything that produces a video stream. For example, the
@racket[color] producer generates a stream of green frames:

@racketmod[
 video (code:comment "green.vid")
 (color "green")]

When the module above is converted into a video, the output
looks something like:

@(inset-flow
  (scale (bitmap (build-path video-dir "scribblings" "sample.png")) 0.3))

This picture also shows an example of playback controls.
These are shown whenever previewing a video. The easiest way
to preview a video is to press the @onscreen{Preview Video}
button in the DrRacket toolbar. Alternatively, the
@exec["raco video"] tool can also preview videos. For
example, say the above video was saved as
@filepath["green.vid"], then the preview can be ran with:
@;
@nested[#:style 'inset]{@exec["raco video --preview green.vid"]}
@;
Note that simply running a
program is not enough to render a video. Every Video
programs describes a single @racket[vid] data structure.
Thus, a renderer (or streamer) can prepare the Video in many
different formats. Additionally, Video programs can include
the @racket[vid] structure from other programs. Evaluating
@racket[vid] in DrRacket's REPL after running the module
shows the resulting structure:

@examples[;#:eval vid-eval
 (eval:alts vid
            ;(playlist (color "green")))]
            (displayln "#<playlist>"))]

The @racket[color] function creates a producer of an
indeterminate length.@margin-note{Although the length can be
 set with the @racket[#:length] keyword.} Another function,
@racket[clip] does create a producer with a bound length.

@racketmod[
 video
 (clip "spinning_square.mp4")]

@inset-flow[
 (apply playlist-timeline the-rr-clip)]

Clips can be further cut with the @racket[#:start] and @racket[#:end] keywords:

@racketmod[
 video
 (clip "spinning_square.mp4"
       #:start 2
       #:end 8)]

@inset-flow[
 (apply playlist-timeline
        (take (drop the-rr-clip 2) 6))]

@section{Filters}

Filters can be attached to every producer. These filters
modify the producers behavior: turning it grayscale,
changing the aspect ratio, etc. The @racket[attach-filter]
function attaches filters to a list. For example, we can use
the @racket[grayscale-filter] to remove the color from the
rotating square clip earlier.

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
provides two ways of combining producers, @emph{playlists}
and @emph{multitracks}. To a first approximation, playlists
run producers sequentially, while multitracks play them together.

Playlists are the simpler form, with each module being an
implicit playlist.

@racketmod[
 video
 (clip "spinning_square.mp4" #:start 0 #:end 4)
 (clip "spinning_square.mp4" #:start 0 #:end 4
       #:filters (list (grayscale-filter)))]
@inset-flow[
 (apply playlist-timeline
        (append (slice the-rr-clip 0 4)
                (slice the-grr-clip 4 8)))]

Playlists are themselves producers. As such,
@racket[playlist] can also append multiple playlists
together. This example combines the playlist from above with
another similar clip of a ball dropping:


@racketmod[
 video
 square-movie
 ball-movie
 (define square-movie
   (playlist
    (clip "spinning_square.mp4" #:start 0 #:end 2)
    (clip "spinning_square.mp4" #:start 2 #:end 4
          #:filters (list (grayscale-filter)))))
 (define ball-movie
   (playlist
    (clip "ball_drop.mp4" #:start 0 #:end 2)
    (clip "ball_drop.mp4" #:start 2 #:end 4
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
 created with @racket[λ/video] and @racket[define/video].}

@section{Transitions}

@racketmod[
 video
 (clip "ball_drop.mp4" #:start 0 #:end 5)
 (fade-transition #:length 2)
 (clip "ball_drop.mp4" #:start 5 #:end 10
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

@section{Multitracks}

@racketmod[
 video
 (multitrack
  (blank #f)
  (composite-transition 0 0 1/2 1)
  (clip "spinning_square.mp4")
  (composite-transition 1/2 0 1/2 1)
  (clip "dropping_ball.mp4"))]

@inset-flow[
 (apply playlist-timeline
        (for/list ([r (in-list the-rr-clip)]
                   [b (in-list the-ball-drop)])
          (shot (hc-append r b))))]

@racketmod[
 video
 (multitrack
  bg
  spinning-square
  dropping-ball
  #:transitions (list (composite-transition 0 0 1/2 1
                                            #:top spinning-square
                                            #:bottom bg)
                      (composite-transition 1/2 0 1/2 1
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

@section[#:tag "raco-video"]{Command Line Interaction}

The @exec{raco video} tool is the most simple way to render
video files. To get a list of its current set of features, run:

@nested[@exec{raco video --help}]

By default, @exec{raco video} will open up a Video file in a
preview window.
