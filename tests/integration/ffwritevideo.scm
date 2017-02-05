(use-modules (oop goops) (aiscm util) (aiscm element) (aiscm v4l2) (aiscm xorg) (aiscm ffmpeg))
(define input (make <v4l2>))
(define output (open-ffmpeg-output (string-append (tmpnam) ".avi") #:shape (shape input) #:frame-rate (/ 99 10)))
(define timer (clock))
(define count 0)
(show
  (lambda _
    (format #t "actual frame rate = ~a frames/second~&" (/ count (elapsed timer)))
    (set! count (1+ count))
    (write-image (read-image input) output)))
(destroy output)
