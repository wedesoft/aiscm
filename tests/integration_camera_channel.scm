(use-modules (oop goops) (aiscm v4l2) (aiscm util))
(define v (make <v4l2> #:device "/dev/video0" #:channel 0))
(grab v)
; #<<image> YUY2 (640 480)>
(destroy v)
