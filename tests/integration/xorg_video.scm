(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda () (grab v)))
(destroy v)
