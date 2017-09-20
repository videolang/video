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

@(require reprovide/reprovide)
@(reprovide (except-in scribble/manual/lang
                       inset-flow
                       image)
            (except-in scribble/manual inset-flow)
            (except-in scribble/core table)
            (except-in pict
                       colorize
                       clip
                       blank)
            scribble/example
            racket/sandbox
            video/base
            video/surface
            video/private/utils
            racket/sandbox
            racket/list
            racket/math
            "utils.rkt"
            (for-label video
                       video/base
                       (except-in racket/base filter #%module-begin)
                       racket/contract/base
                       racket/set
                       racket/hash
                       (except-in racket/class field)
                       racket/gui/base
                       racket/draw
                       video/base
                       (except-in video/core field-element?)
                       video/render
                       video/player
                       video/init))
@(provide (all-defined-out))

@(define video-evaluator
   (make-base-eval
    '(require video/base)))
