(use-modules (oop goops) (aiscm element) (aiscm int) (aiscm pointer) (aiscm sequence) (aiscm pulse) (aiscm util))
(define record (make <pulse-record> #:typecode <sint> #:channels 2 #:rate 44100))
(channels record)
;2
(rate record)
;44100
(define samples (read-samples record (* 3 44100)))
(destroy record)
(define play (make <pulse-play> #:typecode <sint> #:channels 2 #:rate 44100))
(write-samples samples play)
(drain play)
