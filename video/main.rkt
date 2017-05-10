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
         (all-from-out video/base))

(require (prefix-in core: video/core)
         (only-in "private/video.rkt" current-video-directory)
         video/base
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

(define-syntax (λ/video stx)
  (syntax-parse stx
    [(_ args:formals body ...)
     #'(λ args (video-begin "λ/video" values () body ...))]))

(define-syntax (define/video stx)
  (syntax-parse stx
    [(_ args:function-header body ...)
     #'(define f.name (λ/video f.args body ...))]))

(define-syntax (define* stx)
  (syntax-parse stx
    [(_ arg:id body)
     #'(define*-values (arg) body)]))

(define-syntax (define*-values stx)
  (syntax-parse stx
    [(_ (arg:id ...) body)
     (raise-syntax-error 'define* "cannot be used outside of a module or λ/video")]))

(define-syntax (video-begin stx)
  (syntax-parse stx
    [(_ "λ/video" post-process exprs)
     #`(post-process (playlist . #,(reverse (syntax->list #'exprs))))]
    [(_ id:id post-process exprs)
     #`(begin
         (define id (post-process (playlist . #,(reverse (syntax->list #'exprs)))))
         (provide id))]
    [(_ id post-process exprs . body)
     (syntax-parse #'body
       [(b1 . body)
        (define expanded (local-expand #'b1 'module
                                       (append (kernel-form-identifier-list)
                                               (list #'provide #'require #'define*-values))))
        (syntax-parse expanded
          #:literals (begin define*-values)
          [(begin b1 ...)
           #'(video-begin id post-process exprs b1 ... . body)]
          [(define*-values (id* ...) b1)
           #'(splicing-let-values ([(id* ...) b1]) (video-begin id post-process exprs . body))]
          [(id* . rest) ; this bit taken from scribble
           #:when (and (identifier? #'id*)
                       (ormap (lambda (kw) (free-identifier=? #'id* kw))
                              (syntax->list #'(require
                                                provide
                                                define-values
                                                define-syntaxes
                                                begin-for-syntax
                                                module
                                                module*
                                                #%require
                                                #%provide
                                                #%declare))))
           #`(begin #,expanded (video-begin id post-process exprs . body))]
          [_
           #`(video-begin id post-process (#,expanded . exprs) . body)])])]))

