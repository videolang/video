#lang reader video/scribblings/viddoclang

@;{
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
}

@title{Extending Video}

@defmodule[video/surface]

Video also has basic syntax for creating new types of
producers, transitions, and filters.

@colorize[#:color "red"]{
 This API is very experimental and WILL change without
 notice. Proceed with caution. The documentation only lists
 the forms available, as the details themselves will change.}


@defform*[((define-producer)
           (->producer)
           (defproducer)
           (define-transition)
           (->transition)
           (deftransition)
           (define-merge)
           (->merge)
           (defmerge))]{
           
 The forms for defining producers, transitions, and merges.
 Also the forms for building their contracts and writing
 their documentation.}
