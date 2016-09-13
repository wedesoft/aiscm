(use-modules (oop goops) (aiscm element) (aiscm v4l2) (aiscm util))
(define v (make <v4l2>))
(read-image v)
; #<<image> YUY2 (640 480)>
(destroy v)

