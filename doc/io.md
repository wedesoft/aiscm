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

It is also possible to specify the device, a channel, and a closure for selecting the video mode.

```Scheme
(use-modules (oop goops) (ice-9 rdelim) (aiscm v4l2) (aiscm util))
(define (select formats)
  (for-each (lambda (i mode) (format #t "~a: ~a~&" i mode))
            (iota (length formats))
            formats)
  (format #t "> ") (list-ref formats (string->number (read-line (current-input-port)))))
(define v (make <v4l2> #:device "/dev/video0" #:channel 0 #:select select))
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

One can also display a list of images:

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm sequence) (aiscm pointer) (aiscm jit) (aiscm rgb) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(define img (to-array (grab v)))
(show (list (red img) (green img) (blue img)))
(destroy v)
```

It is also possible to display a video using the *show* command:

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda () (grab v)))
(destroy v)
```

A function returning lists of images can be used to display multiple videos synchronously.

```Scheme
(use-modules (oop goops) (aiscm v4l2) (aiscm sequence) (aiscm pointer) (aiscm jit) (aiscm rgb) (aiscm xorg) (aiscm util))
(define v (make <v4l2>))
(show (lambda () (let [(img (to-array (grab v)))] (list (red img) (green img) (blue img)))))
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
(do () ((quit? d)) (show w (grab v)) (process-events d))
(quit= d #f)
(hide w)
(destroy d)
(destroy v)
```

## Loading/saving images using ImageMagick

An image can be read from disk using *read-image* which uses the ImageMagick library to load the image.

```Scheme
(use-modules (oop goops) (aiscm magick) (aiscm sequence) (aiscm pointer) (aiscm xorg))
(show (read-image "tests/fixtures/index.png"))
```

Analogous *write-image* writes an image to disk.

```Scheme
(use-modules (oop goops) (aiscm magick) (aiscm sequence) (aiscm pointer) (aiscm xorg))
(write-image (arr (1 2 3 4) (5 6 7 8)) "/tmp/test.png")
```

## Pulse audio

The following example program creates a sine wave and outputs it to the audio device.

```Scheme
(use-modules (oop goops) (aiscm int) (aiscm pointer) (aiscm sequence) (aiscm pulse) (aiscm util))
(define samples (to-array <sint> (map (lambda (t) (round (* (sin (/ (* t 1000 2 3.1415926) 44100)) 20000))) (iota 441))))
(define play (make <pulse-play> #:type <sint> #:channels 1 #:rate 44100))
(for-each (lambda (i) (write-samples samples play)) (iota 100))
(drain play)
(destroy play)
```

Audio data can be recorded in a similar fashion

```Scheme
(use-modules (oop goops) (aiscm int) (aiscm pointer) (aiscm sequence) (aiscm pulse) (aiscm util))
(define record (make <pulse-record> #:type <sint> #:channels 1 #:rate 44100))
(define samples (read-samples record 44100))
(display samples)
(destroy record)
```
