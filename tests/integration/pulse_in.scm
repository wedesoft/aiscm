(use-modules (oop goops) (aiscm element) (aiscm int) (aiscm pointer) (aiscm sequence) (aiscm pulse) (aiscm util))
(define record (make <pulse-record> #:type <sint> #:channels 1 #:rate 44100))
(channels record)
;1
(rate record)
;44100
(define samples (read-samples record 44100))
(display samples)
(destroy record)
