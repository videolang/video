#lang video

(clip "vid.mp4"
      #:properties (hash "start" 50
                         "end" (if (equal? (get-property v-clip "vid-key") "block")
                                   200
                                   51)))

(define v-clip
  (clip "vid.mp4"
        #:properties (hash "vid-key" "block")))
