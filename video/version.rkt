#lang racket/base

#|
   Copyright 2016-2018 Leif Andersen

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

(require racket/contract/base)
(provide (contract-out [version (-> string?)]))

(define current-version
  (mk-version #:major "0"
              #:minor "2"
              #:patch "3"
              #:pre-release #f))

(define (version)
  (version->string current-version))

(module version-infra racket/base
  (provide (all-defined-out))
  (struct version (major
                   minor
                   patch
                   pre-release))
  (define (mk-version #:major [major #f]
                      #:minor [minor #f]
                      #:patch [patch #f]
                      #:pre-release [pre-release #f])
    (version major minor patch pre-release))
  (define (version->string v)
    (define major (or (version-major v) "0"))
    (define minor (or (version-minor v) "0"))
    (define patch (or (version-patch v) "0"))
    (define pre-release (version-pre-release v))
    (if pre-release
        (format "~a.~a.~a-~a" major minor patch pre-release)
        (format "~a.~a.~a" major minor patch))))
(require 'version-infra)
