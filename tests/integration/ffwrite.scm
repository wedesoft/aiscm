(use-modules (oop goops) (aiscm ffmpeg) (aiscm v4l2) (aiscm pulse) (aiscm xorg) (aiscm element) (aiscm int))
(define input (make <v4l2>))
(define output (open-ffmpeg-output (string-append (tmpnam) ".avi")
                                   #:shape (shape input) #:frame-rate 7 #:rate 44100 #:typecode <sint> #:channels 2))
(define record (make <pulse-record> #:typecode <sint> #:channels 2 #:rate 44100))
(show
  (lambda _
    (write-audio (read-audio record (/ (rate output) (frame-rate output))) output)
    (write-image (read-image input) output)))
(destroy output)
