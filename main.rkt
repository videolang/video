#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [~module-begin #%module-begin])
         (all-from-out video/base))

(require (prefix-in core: video/core)
         (except-in video/core blank)
         video/base
         racket/list
         (except-in pict clip frame blank)
         racket/draw
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/kerncase))

(define-syntax (~module-begin stx)
  (syntax-parse stx
    [(_ id:id post-process exprs . body)
     #'(#%module-begin
        (video-begin id post-process exprs . body))]))

(define-syntax (video-begin stx)
  (syntax-parse stx
    [(_ id:id post-process exprs)
     #`(begin
         (define id (post-process (list . #,(reverse (syntax->list #'exprs)))))
         (provide id))]
    [(_ id:id post-process exprs . body)
     (syntax-parse #'body
       [(b1 . body)
        (define expanded (local-expand #'b1 'module
                                       (append (kernel-form-identifier-list)
                                               (list #'provide #'require))))
        (syntax-parse expanded
          #:literals (begin)
          [(begin b1 ...)
           #'(video-begin id post-process exprs b1 ... . body)]
          [(id . rest) ; this bit taken froms cribble
           #:when (and (identifier? #'id)
                       (ormap (lambda (kw) (free-identifier=? #'id kw))
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
