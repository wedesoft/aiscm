(use-modules (oop goops) (aiscm ffmpeg) (aiscm pulse) (aiscm element))
(define audio (open-ffmpeg-input "test.mp3"))
(define output (make <pulse-play> #:rate (rate audio) #:channels (channels audio) #:type (typecode audio)))
(write-samples
  (lambda _
    (format #t "audio pts = ~8,2f~&" (audio-pts audio))
    (read-audio audio))
  output)
(drain output)
