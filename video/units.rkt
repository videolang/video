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


;; The Units library is still being designed. The idea is
;;    to have Video do the dimentional analysis for you.
;; For example, add two minutes to 30 seconds should return
;;    either 150 seconds or 2.5 (exact) minutes.

(require racket/contract/base
         "private/units.rkt")
(provide
 (contract-out [pixels? (-> any/c boolean?)]
               [seconds? (-> any/c boolean?)]
               [dB? (-> any/c boolean?)]
               [pixels (-> number? pixels?)]
               [seconds (-> number? seconds?)]
               [dB (-> number? dB?)]))
