#lang s-exp syntax/module-reader

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

#:read scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:info        (scribble-base-reader-info)
#:language (build-path this-dir "templates.rkt")
#:wrapper1 (lambda (t) (cons 'doc (t)))

(require (prefix-in scribble: scribble/reader)
         (only-in scribble/base/reader scribble-base-reader-info)
         (only-in racket/runtime-path define-runtime-path)
         (for-syntax (only-in racket/base #%datum)))

(define-runtime-path this-dir ".")
