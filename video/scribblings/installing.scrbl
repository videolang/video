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

@title{Installing}

Video uses FFmpeg as part of its back-end. The language
includes FFmpeg for macOS and Windows. Linux (and other OS)
users will need to install FFmpeg themselves. Custom FFmpeg
builds are also an option when needed. Details are provided
for Windows/macOS/Linux.

@section{Windows}

Video for Windows comes bundled with FFmpeg. You can
optionally install a different version of FFmpeg. To install
a custom FFmpeg build, open the environment variables window
and add the path to the DLLs to the your library path. Make
sure to use at least the minimum version of FFmpeg as
documented in @secref["ffmpeg-specs"].

@section{macOS}

You're good to go.

@section{Linux}

You need to install ffmpeg.

@section[#:tag "ffmpeg-specs"]{FFmpeg Requirements}

Video requires at least FFmpeg 3.2, and recommends FFmpeg
3.3. The full requirements are as follows:

Minimum Version:

@inset-flow[
 @tabular[#:style 'boxed
          #:column-properties '(left right)
          `(("avutil" "55.58")
            ("libavcodec" "57.89")
            ("libavformat" "57.71")
            ("libavfilter" "6.82")
            ("libswscale" "4.6")
            ("libswresample" "2.7")
            ("libavdevice" "57.6"))]]

Recommended Version:

@inset-flow[
 @tabular[#:style 'boxed
          #:column-properties '(left right)
          `(("libavutil" "55.58")
            ("libavcodec" "57.89")
            ("libavformat" "57.71")
            ("libavfilter" "6.82")
            ("libswscale" "4.6")
            ("libswresample" "2.7")
            ("libavdevice" "57.6"))]]

Note that the miner version is a minimum, while the major
version is exact. Video has this requirement because major
versions of FFmpeg libraries breaks backwards compatibility.
