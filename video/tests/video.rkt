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
         "../private/video.rkt"
         (prefix-in surf: "../base.rkt"))

;; Test syntax for the video object hiarachy.
(let ()
  (define-constructor new-video #f () ())
  (define-constructor new-sub new-video ([hello #f]) ())
  (define-constructor sub-sub new-sub ([world #f]) ())
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
  (check-not-equal? ss3 ss)
  (check-not-equal? ss3 ss2))

;; Test the basic constructors for video types.
(let ()
  (make-properties)
  (make-service)
  (make-filter)
  (make-transition)
  (make-producer)
  (make-blank)
  (make-playlist)
  (make-multitrack)
  (make-field-element)
  (make-chapter)
  (make-video-subgraph)
  (void))

;; Check video properties
(let ()
  (define c (surf:color "blue" #:properties (hash "bluecolor" 42)))
  (define long-c (remove-property c "bluecolor"))
  (check-true (dict-has-key? c "bluecolor"))
  (check-false (dict-has-key? long-c "bluecolor"))
  (check-equal? (get-property c "bluecolor") 42)
  (check-equal? (get-property long-c "bluecolor" (λ () 1337)) 1337))

;; Check printing
(let ()
  (define c (surf:color "blue"))
  (check-equal?
   (with-output-to-string
       (λ ()
         (display c)))
   "#<producer>")
  ;; Yes, not a perfect match, but it should be good enough
  ;;  for a simple test
  (check-regexp-match
   "#<producer:[^>]*>"
   (with-output-to-string
       (λ ()
         (parameterize ([current-detailed-printing? #t])
           (display c))))))

