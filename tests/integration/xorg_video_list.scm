(use-modules (oop goops) (aiscm v4l2) (aiscm sequence) (aiscm pointer) (aiscm jit) (aiscm rgb) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda _ (let [(img (to-array (grab v)))] (list (red img) (green img) (blue img)))))
(destroy v)
