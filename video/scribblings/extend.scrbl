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

@defmodule[video/convert]

Video offers several extension points. Both for adding new
syntactic forms into the language, as well as new video
primitives. There is some overlap in what the extensions are
capable of, but each are useful in different circumstances.

@colorize[#:color "red"]{
 The documentation in this section needs a lot of work.}

@section{Video Convert}

@defproc[(video-convertible? [obj any/c])
         boolean?]
@defproc[(video-convert [obj any/c])
         video?]
@defthing[prop:video-convertible struct-type-property?]
@defthing[prop:video-convertible? struct-type-property?]

@section{Conversion Database}

@defclass[convert-database% object% ()]{
 @defmethod[(register-conversion [predicate (-> any/c boolean?)]
                                 [converter (-> any/c video?)])
            video?]
 @defmethod[(convert [object any/c])
            video?] 
 @defmethod[(maybe-convert [object any/c])
            (or/c video? #f)]
 @defmethod[(convertible? [object any/c])
            boolean?]}

@section{Core Forms}

@defmodule[video/surface]

Video also has basic syntax for creating new types of
producers, transitions, merges, and filters. This API is still
experimental and may change with little notice. Proceed with
caution.

These forms are for defining producers, filters, transitions, and merges.
Also the forms for building their contracts and writing
their documentation.

@defform[(define-producer function-header
           maybe-subgraph
           maybe-properties
           maybe-user-properties
           body ...)
         #:grammar
         [(maybe-subgraph (code:line)
                          (code:line #:subgraph subgraph))
          (maybe-properties (code:line)
                            (code:line #:properties properties))
          (maybe-user-properties (code:line)
                                 (code:line #:user-properties user-properties))]
         #:contracts ([subgraph (or/c procedure? #f)]
                      [properties procedure?])]{
}

@defform[(->producer [extra-args ...] [optional-args ...] maybe-ret)
         #:grammar
         [(maybe-ret (code:line)
                     return)]]{
}

@defform[(defproducer prototype maybe-return content ...)
         #:grammar
         [(prototype (id arg-spec ...)
                     (prototype arg-spec ...))
          (maybe-ret (code:line)
                     #:return ret)]]{
}

@defform[(define-filter function-header
           maybe-subgraph
           maybe-properties
           maybe-source-properties
           maybe-user-properties
           body ...)
         #:grammar
         [(maybe-subgraph (code:line)
                          (code:line #:subgraph subgraph))
          (maybe-properties (code:line)
                            (code:line #:properties properties))
          (maybe-user-properties (code:line)
                                 (code:line #:user-properties user-properties))
          (maybe-source-properties (code:line)
                                   (code:line #:source-props source-properties))]
         #:contracts ([subgraph (or/c procedure? #f)]
                      [properties procedure?]
                      [source-props procedure?])]{
}

@defform[(->filter [extra-args ...] [optional-args ...] maybe-ret)
         #:grammar
         [(maybe-ret (code:line)
                     return)]]{
}

@defform[(deffilter prototype maybe-return content ...)
         #:grammar
         [(prototype (id arg-spec ...)
                     (prototype arg-spec ...))
          (maybe-ret (code:line)
                     #:return ret)]]{
}

@defform[(define-transition function-header
           maybe-track1-subgraph
           maybe-track2-subgraph
           maybe-combined-subgraph
           maybe-properties
           maybe-source-properties
           maybe-user-properties
           maybe-producer-1
           maybe-producer-2
           body ...)
         #:grammar
         [(maybe-track1-subgraph (code:line)
                                 (code:line #:track1-subgraph subgraph))
          (maybe-track2-subgraph (code:line)
                                 (code:line #:track2-subgraph subgraph))
          (maybe-combined-subgraph (code:line)
                                   (code:line #:combined-subgraph subgraph))
          (maybe-properties (code:line)
                            (code:line #:properties properties))
          (maybe-user-properties (code:line)
                                 (code:line #:user-properties user-properties))
          (maybe-source-properties (code:line)
                                   (code:line #:source-props source-properties))
          (maybe-prod1 (code:line)
                       (code:line #:prod-1 prod1-id))
          (maybe-prod2 (code:line)
                       (code:line #:prod-2 prod2-id))]
         #:contracts ([subgraph (or/c procedure? #f)]
                      [properties procedure?]
                      [source-props procedure?])]{
}

@defform[(->transition [extra-args ...] [optional-args ...] maybe-ret)
         #:grammar
         [(maybe-ret (code:line)
                     return)]]{
}

@defform[(deftransition prototype maybe-return content ...)
         #:grammar
         [(prototype (id arg-spec ...)
                     (prototype arg-spec ...))
          (maybe-ret (code:line)
                     #:return ret)]]{
}

@defform[(define-merge function-header
           maybe-track1-subgraph
           maybe-track2-subgraph
           maybe-combined-subgraph
           maybe-properties
           maybe-source-properties
           maybe-user-properties
           maybe-producer-1
           maybe-producer-2
           body ...)
         #:grammar
         [(maybe-track1-subgraph (code:line)
                                 (code:line #:track1-subgraph subgraph))
          (maybe-track2-subgraph (code:line)
                                 (code:line #:track2-subgraph subgraph))
          (maybe-combined-subgraph (code:line)
                                   (code:line #:combined-subgraph subgraph))
          (maybe-properties (code:line)
                            (code:line #:properties properties))
          (maybe-user-properties (code:line)
                                 (code:line #:user-properties user-properties))
          (maybe-source-properties (code:line)
                                   (code:line #:source-props source-properties))
          (maybe-prod1 (code:line)
                       (code:line #:prod-1 prod1-id))
          (maybe-prod2 (code:line)
                       (code:line #:prod-2 prod2-id))]
         #:contracts ([subgraph (or/c procedure? #f)]
                      [properties procedure?]
                      [source-props procedure?])]{
}

@defform[(->merge [extra-args ...] [optional-args ...] maybe-ret)
         #:grammar
         [(maybe-ret (code:line)
                     return)]]{
}

@defform[(defmerge prototype maybe-return content ...)
         #:grammar
         [(prototype (id arg-spec ...)
                     (prototype arg-spec ...))
          (maybe-ret (code:line)
                     #:return ret)]]{
}

