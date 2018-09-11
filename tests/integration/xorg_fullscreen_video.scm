(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm core))
(define v (make <v4l2>))
(show (lambda _ (read-image v)) #:fullscreen #t)
(destroy v)
