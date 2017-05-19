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

;; This is an experimental renderer for h.26x. It is still very incomplete.
;; All I guarentee at the moment is that it doesn't make video crash on installation.
;; Hopefully with time this will get moved out of the experimental section.

(require bitsyntax)

(define PSC (integer->bit-string 20 #b00000000000000010000 #t))
(define GBSC (integer->bit-string 16 #b0000000000000001 #t))
