#lang scribble/manual
@require[@for-label[video/core
                    video/render]]

@title{Video}
@author{leif}

This is a highly unstable and experimental DSL for editing
videos. Do not use this library as core parts of it are
still being written.

This libraries requires that you have
@hyperlink["https://mltframework.org/"]{libmlt} installed on
your system

@section{Rendering}
@defmodule[video/render]

Currently only one function is supported:

@defproc[(render [data video?])
         void?]{
 Renders a video object using mlt. Examples
 of how to use it in @tt{private/examples.rkt}.}

@section{Core Library}
@defmodule[video/core]

The structs given to the @racket[render] function are found
here. Examples are found in @tt{private/examples.rkt}.
