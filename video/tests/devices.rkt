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

(require rackunit
         "test-utils.rkt"
         (prefix-in d: "../private/devices.rkt")
         "../devices.rkt")

(check-true (input-devices? (d:mk-input-devices)))

(let ()
  (define devices (list-input-devices))
  (check-true (input-devices? devices))
  (cameras devices)
  (video-devices devices)
  (audio-devices devices)
  (void))
