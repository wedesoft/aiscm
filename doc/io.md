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

## Xorg display

You can capture an image from a camera and display it using *show* as follows:

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (grab v))
(destroy v)
```

It is also possible to display a video using the *show* command:

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda () (grab v)))
(destroy v)
```

If necessary, one can also handle the display and window objects directly.
Possible types of output are *IO-XIMAGE*, *IO-OPENGL*, and *IO-XVIDEO*.

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(define d (make <xdisplay> #:name ":0.0"))
(define w (make <xwindow> #:display d #:shape '(640 480) #:io IO-XVIDEO))
(title= w "Test")
(show w)
(do () ((quit? d)) (write w (grab v)) (process-events d))
(quit= d #f)
(hide w)
(destroy d)
(destroy v)
```
