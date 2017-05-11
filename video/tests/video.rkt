#lang racket

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
         (prefix-in file: file/convertible)
         "../private/video.rkt")

(let ()
  (define-constructor new-video #f ())
  (define-constructor new-sub new-video ([hello #f]))
  (define-constructor sub-sub new-sub ([world #f]))
  (define ss (make-sub-sub))
  (define ss2
    (make-sub-sub #:hello "HI"
                  #:world "YOU"))
  (check-true (sub-sub? ss))
  (check-false (new-sub-hello ss))
  (check-false (sub-sub-world ss))
  (check-true (sub-sub? ss2))
  (check-equal? (new-sub-hello ss2) "HI")
  (check-equal? (sub-sub-world ss2) "YOU")
  (check-false (file:convert (make-new-video) 'mlt))
  (define ss3 (copy-video ss))
  (check-equal? ss3 ss)
  (check-not-equal? ss3 ss2))

(let ()
  (make-link)
  (make-properties)
  (make-anim-property)
  (make-service)
  (make-filter)
  (make-transition)
  (make-producer)
  (make-blank)
  (make-playlist)
  (make-playlist-producer)
  (make-multitrack)
  (make-field-element)
  (void))
