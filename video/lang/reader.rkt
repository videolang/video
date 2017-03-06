#lang s-exp syntax/module-reader
video

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

#:read read
#:read-syntax read-syntax
#:wrapper1 (λ (x) (list* 'vid 'values '() (x)))
#:info make-info

(require scribble/reader)

(define (make-info key default use-default)
  (case key
    [(drracket:toolbar-buttons)
     (define camera-button
       (dynamic-require 'video/private/camera-icon 'camera-button))
     (list camera-button)]
    [else (use-default key default)]))
