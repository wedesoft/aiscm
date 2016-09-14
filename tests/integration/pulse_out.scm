(use-modules (oop goops) (aiscm pulse) (aiscm element) (aiscm int) (aiscm pointer) (aiscm sequence))
(define samples (to-array <sint> (map (lambda (t) (round (* (sin (/ (* t 1000 2 3.1415926) 44100)) 20000))) (iota 441))))
(define output (make <pulse-play> #:typecode <sint> #:channels 1 #:rate 44100))
(channels output)
;1
(rate output)
;44100
(for-each (lambda _ (write-audio samples output)) (iota 300))
(drain output)
(destroy output)
