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

;; This module combines all of the ffmpeg bindings for use by other modules
;;   in the video package.
;; The bindings were split up for two reasons:
;; (A). Being in one module lead to an very large file
;; (B). Splitting it up makes it easier to require on part without needing
;;      to require the rest of the bindings.

(provide (all-defined-out)
         (all-from-out "ffmpeg.rkt")
         (all-from-out "lib.rkt")
         (all-from-out "constants.rkt")
         (all-from-out "data.rkt"))
(require "ffmpeg.rkt"
         "lib.rkt"
         "constants.rkt"
         "data.rkt")
