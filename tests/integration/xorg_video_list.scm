(use-modules (oop goops) (aiscm v4l2) (aiscm core) (aiscm image) (aiscm xorg))
(define v (make <v4l2>))
(show (lambda _ (let [(img (from-image (read-image v)))] (list (red img) (green img) (blue img)))))
(destroy v)
