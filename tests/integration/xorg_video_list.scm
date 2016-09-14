(use-modules (oop goops) (aiscm element) (aiscm v4l2) (aiscm sequence) (aiscm jit) (aiscm rgb) (aiscm xorg))
(define v (make <v4l2>))
(show (lambda _ (let [(img (to-array (read-image v)))] (list (red img) (green img) (blue img)))))
(destroy v)
