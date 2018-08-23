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

;; This library is still experimental, and the workings of
;; the API are liekly to change

(require "private/surface.rkt"
         (except-in "private/ffmpeg-pipeline.rkt" filter?))
(provide define-producer
         ->producer
         defproducer
         producer?
         define-filter
         ->filter
         deffilter
         filter?

         ;; Transitions define three procs:
         ;; track1-subgraph : Graph Node -> (U (vertex-subgraph #:source Graph
         ;;                                                     #:sources Node
         ;;                                                     #:sinks Node)
         ;;                                     #f)
         ;; track2-subgraph : Graph Node -> (U (vertex-subgraph #:source Graph
         ;;                                                     #:sources Node
         ;;                                                     #:sinks Node)
         ;;                                     #f)
         ;; combined-subgraph : Graph Node Node -> (U (vertex-subgraph #:source Graph
         ;;                                                            #:sources (cons Node Node)
         ;;                                                            #:sinks Node)
         ;;                                            #f)
         ;; 
         ;; In all cases the Graph is a context which the transitions can insert nodes
         ;;   into. If they use this graph, they should return it as #:source. Alternatively,
         ;;   a transition can create its own graph and return it.
         ;;
         ;; The track1-subgraph and track2-subgraph procedures are given a pointer to the node
         ;;   that will feed into it. Do NOT connect any of the subgraphs nodes to it, that is handled
         ;;   by the runtime. Return the source and sink of the graph in the appropriate fieldds.
         ;;
         ;; The combined-subgraph procedure is the same as the first two, except
         ;;   it takes two sources, one for node-a and one for node-b. As such,
         ;;   the #:sources field should be a PAIR of two nodes. It is connected
         ;;   to the surrounding graph in the same fashion as the other two
         ;;   procedures.
         ;;
         ;; Alternatively, the procedure can return #f. In this case, no
         ;;   subgraph is inserted into the render graph. Instead, the source
         ;;   is dumped directly into a black hole sink. This is useful if the
         ;;   resulting node should NOT be inserted into its external context.
         ;;
         ;; Frequently, either combined-subgraph or both track1-subgraph
         ;;    and track2-subgraph will return #f.
         ;;
         ;; For any procedure that returns a subgraph, it must fill in `length`
         ;;   in the properties field. For track1-subgraph and track2-subraph,
         ;;   this length is subtracted from the total length of the playlsit/multitrack.
         ;;   For combined-subgraph, this length is ADDED to the length of the
         ;;   playlist/multitrack. This allows producers to remove different
         ;;   amounts of their surrounding clips.
         ;;
         ;; Additionally, in order to be used in a playlist, the properties
         ;;   table for a transition (NOT the subgraph procs) MUST define
         ;;   `pre-length` and `post-length`. This is used to determine
         ;;   the time expected to be surrounding the transition. This is
         ;;   not needed for transitions used in multitracks.
         define-transition
         ->transition
         deftransition
         transition?

         ;; Merges have the same API as transitions. The only
         ;;   diference is they have the keywords #:top #:bottom
         ;;   rather than up/down
         define-merge
         ->merge
         defmerge
         merge?

         ;; The following functions define videograph (and some filtergraph)
         ;;   level primitives to use in transitions/mergers/filters.
         node-counts
         node-props
         filter-node?
         mk-filter-node
         mux-node?
         mk-mux-node
         mk-empty-sink-video-filter
         mk-empty-sink-audio-filter
         mk-empty-sink-node
         mk-empty-video-filter
         mk-empty-audio-filter
         mk-empty-node
         mk-fifo-video-filter
         mk-fifo-audio-filter
         mk-fifo-node
         mk-split-video-filter
         mk-split-audio-filter
         mk-split-node
         mk-trim-video-filter
         mk-trim-audio-filter
         mk-trim-node
         mk-reset-timestamp-video-filter
         mk-reset-timestamp-audio-filter
         mk-reset-timestamp-node
         mk-filter
         color->string)
