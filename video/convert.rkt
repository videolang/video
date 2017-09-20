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

(require racket/contract/base
         racket/class
         racket/set
         racket/file
         (prefix-in pict: pict)
         "private/video.rkt")
(provide convert-database%
         (contract-out
          [make-base-database (-> (is-a?/c convert-database%))]
          [video-convert (-> video-convertible? video?)]
          [video-convertible? (-> any/c boolean?)]
          [prop:video-convertible (struct-type-property/c (-> video-convertible? video?))]
          [prop:video-convertible? (struct-type-property/c predicate/c)]))

;; The conversion database is used by the renderer to
;;   add conversions that objects themselves don't know
;;   how to convert to Videos. For example, picts can
;;   be added to this database.
(define convert-database%
  (class object%
    (super-new)

    (define the-database (mutable-set))
    
    ;; Register a new type of conversion with the database.
    ;; The conversion can handle anything where pred evaluates to
    ;;   #t, and the conversion itself is stored as convert
    (define/public (register-conversion pred convert)
      (set-add! (cons pred convert)))
    
    ;; Return #t if `obj` is convertible, #f otherwise.
    (define/public (convertible? obj)
      (for/fold ([c? #f])
                ([i (in-set the-database)])
        #:break c?
        (or c? ((car i) obj))))

    ;; Query the database to see if any conversions work,
    ;;   if so, apply the conversion and return the result,
    ;;   otherwise return #f.
    (define/public (maybe-convert obj)
      (for/fold ([val #f])
                ([i (in-set the-database)])
        #:break val
        (and ((car i) obj)
             ((cdr i) obj))))

    ;; Like maybe-convert, but always return a result or
    ;;   throw an error on failure
    (define/public (convert obj)
      (define ret (maybe-convert obj))
      (unless ret
        (error 'conversion-table "No registered conversion for: ~a" obj))
      ret)))

;; Create a database with known types.
;; Perhaps this function will one day be moved to another
;;   library outside of the core of video.
(define (make-base-database)
  (define the-database (new convert-database%))
  (send the-database register-conversion
        pict:pict?
        (λ (p)
          (define b (pict:pict->bitmap p))
          (define res (make-temporary-file "pict~a.jpg"))
          (send b save-file res 'png 100)
          (make-file #:path res)))
  the-database)

(define convert-database<%>/c
  (class/c [register-conversion (->m (-> any/c boolean?) (-> any/c video?) void?)]
           [maybe-convert (->m any/c (or/c video? #f))]
           [convertible? (->m any/c boolean?)]
           [convert (->m any/c video?)]))
