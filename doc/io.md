# Input/output

## Video for Linux version 2 (V4L2)

You can open a camera and grab a frame as follows.

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm util))
(define v (make <v4l2>))
(grab v)
; #<<image> YUY2 (640 480)>
(destroy v)
```

The video device and channel can be specified like this.

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm util))
(define v (make <v4l2> #:device "/dev/video0" #:channel 0))
(grab v)
; #<<image> YUY2 (640 480)>
(destroy v)
```

It is also possible to specify a closure for selecting the video mode.

```Scheme
(use-modules (oop goops) (ice-9 rdelim) (aiscm v4l2) (aiscm util))
(define (select formats)
  (for-each (lambda (i mode) (format #t "~a: ~a~&" i mode))
            (iota (length formats))
            formats)
  (format #t "> ")
  (list-ref formats (string->number (read-line (current-input-port)))))
(define v (make <v4l2> #:select select))
(grab v)
(destroy v)
```
