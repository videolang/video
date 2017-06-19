#lang info

#|
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
|#

(define collection 'multi)

(define deps '(("base" "6.10.0.6")
                "rackunit-lib"
                "gui-lib"
                "draw-lib"
                "images-lib"
                "drracket-plugin-lib"
                "data-lib"
                "pict-lib"
                "wxme-lib"
                "sandbox-lib"
                "at-exp-lib"
                "scribble-lib"
                "bitsyntax"
                "opengl"
                "portaudio"
                "graph"
                ("ffmpeg-x86_64-macosx" #:platform "x86_64-macosx")
                ("ffmpeg-x86_64-win32" #:platform "win32\\x86_64")
                ("ffmpeg-i386-win32" #:platform "win32\\i386")))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "gui-doc"
                     "draw-doc"
                     "ppict"))

(define version "0.2")
(define pkg-authors '(leif))
(define pkg-desc "Video Language")
