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

(require racket/stxparam
         rackunit
         ffi/unsafe
         "../private/init-mlt.rkt"
         "../private/mlt.rkt")

(syntax-parameterize ([current-func-name (make-rename-transformer #'hello)])
  (ret-error #f)
  (check-equal?
   (null-error #t)
   #t)
  (check-equal?
   (null-error "hello")
   "hello")
  (check-exn
   exn:fail:mlt?
   (λ () (ret-error #t)))
  (check-exn
   exn:fail:mlt?
   (λ () (null-error #f))))

(parameterize ([raise-mlt-warnings #f])
  (raise-mlt-warning test))

(parameterize ([raise-mlt-warnings #t])
  (check-exn
   exn:fail:mlt:warning?
   (λ () (raise-mlt-warning test))))

(let ()
  (define-mlt* mlt-factory-init
    (_fun _path -> _mlt-repository/null))
  (void))

(let ()
  (define prof (mlt-profile-init #f))
  (set-mlt-profile-description! prof #f)
  (set-mlt-profile-progressive! prof 1)
  (set-mlt-profile-sample-aspect-num! prof 1)
  (set-mlt-profile-sample-aspect-den! prof 1)
  (set-mlt-profile-display-aspect-num! prof 1)
  (set-mlt-profile-display-aspect-den! prof 1)
  (set-mlt-profile-colorspace! prof 1)
  (set-mlt-profile-is-explicit! prof 1)
  (let ()
    (define prod
      (mlt-factory-producer prof #f "color:green"))
    (define props
      (mlt-producer-properties prod))
    (set-mlt-properties-child! props #f)
    (set-mlt-properties-local! props #f)
    (set-mlt-properties-close-object! props #f)
    (mlt-producer-close prod))
  (let ()
    (define track (mlt-tractor-new))
    (define mtt (mlt-tractor-multitrack track))
    (set-mlt-multitrack-list*! mtt #f)
    (set-mlt-multitrack-size*! mtt 0)
    (set-mlt-multitrack-count*! mtt 0)
    (mlt-tractor-close track))
  (let ()
    (define pl (mlt-playlist-init))
    (set-mlt-playlist-blank*! pl (mlt-playlist-blank* pl))
    (set-mlt-playlist-size*! pl 0)
    (set-mlt-playlist-count*! pl 0)
    (set-mlt-playlist-list*! pl (mlt-playlist-list* pl))
    (define prod (mlt-factory-producer prof #f "color:red"))
    (mlt-playlist-append pl prod)
    (define pinfo (mlt-playlist-get-clip-info pl 0))
    (set-mlt-playlist-clip-info-clip! pinfo 0)
    (set-mlt-playlist-clip-info-producer! pinfo (mlt-playlist-clip-info-producer pinfo))
    (set-mlt-playlist-clip-info-cut! pinfo #f)
    (set-mlt-playlist-clip-info-start! pinfo 0)
    (set-mlt-playlist-clip-info-resource! pinfo #f)
    (set-mlt-playlist-clip-info-frame-in! pinfo 0)
    (set-mlt-playlist-clip-info-frame-out! pinfo 0)
    (set-mlt-playlist-clip-info-frame-count! pinfo 0)
    (set-mlt-playlist-clip-info-length! pinfo 0)
    (set-mlt-playlist-clip-info-fps! pinfo 0.0)
    (set-mlt-playlist-clip-info-repeate! pinfo 0)
    (mlt-playlist-clear pl)
    (mlt-producer-close prod)
    (mlt-playlist-close pl)
    (mlt-profile-close prof)))
  