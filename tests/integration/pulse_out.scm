(use-modules (oop goops) (aiscm pulse) (aiscm core))
(define sine (map (lambda (t) (inexact->exact (round (* (sin (/ (* t 1000 2 3.1415926) 44100)) 20000)))) (iota 441)))
(define samples (to-array <sint> sine))
(define output (make <pulse-play> #:typecode <sint> #:channels 1 #:rate 44100))
(channels output)
;1
(rate output)
;44100
(for-each (lambda _ (write-audio samples output)) (iota 300))
(drain output)
