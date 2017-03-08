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
         @for-label[video
                    video/base]]

@title{Getting Started}

Video is a functional-declarative language for manipulating
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

All video programs begin with @code{#lang video}, the
remaining program is an interleaving of video description.

@racketmod[
 video
 (color "green")]

@(scale (bitmap (build-path video-dir "scribblings" "sample.png")) 0.3)
