#lang racket/base

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

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [~module-begin #%module-begin])
         λ/video
         define/video
         define*
         define*-values
         (all-from-out "base.rkt"))

(require (prefix-in core: video/core)
         (only-in "private/video.rkt" current-video-directory)
         "private/lang.rkt"
         "base.rkt"
         racket/list
         racket/path
         (except-in pict clip frame blank)
         racket/draw
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/kerncase))

(define-syntax (~module-begin stx)
  (syntax-parse stx
    [(_ id:id post-process exprs . body)
     #'(#%module-begin
        (current-video-directory
         (let ([p (variable-reference->module-source (#%variable-reference))])
           (if (path? p)
               (path-only p)
               (current-directory))))
        (video-begin id post-process exprs . body))]))
