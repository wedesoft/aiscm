(use-modules (oop goops) (aiscm element) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda _ (read-image v)))
(destroy v)
