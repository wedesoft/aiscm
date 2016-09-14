(use-modules (oop goops) (aiscm v4l2))
(define v (make <v4l2>))
(read-image v)
; #<<image> YUY2 (640 480)>
(destroy v)

