#lang reader video/scribblings/viddoclang

@;{
   Copyright 2016-2018 Leif Andersen

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

@title{The Video Language Guide (v@video:version{})}
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

@include-section{installing.scrbl}
@include-section{intro.scrbl}
@include-section{api.scrbl}
@include-section{player.scrbl}
@include-section{render.scrbl}
@include-section{graphical.scrbl}
@include-section{core.scrbl}
@include-section{extend.scrbl}

@index-section[]
