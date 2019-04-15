(use-modules (oop goops) (ice-9 format) (aiscm core) (aiscm pulse) (aiscm ffmpeg))

(define rate 11025)
(define chunk 512)
(define seconds 300)
(define count (inexact->exact (* chunk (ceiling (/ (* rate seconds) chunk)))))

(format #t "Recording ~a seconds of background noise~&" seconds)
(define record (make <pulse-record> #:typecode <sint> #:channels 1 #:rate rate))
(define output (open-ffmpeg-output "background.mp3" #:rate rate #:typecode <sint> #:channels 1 #:audio-bit-rate 80000))
(define samples (read-audio record count))
(write-audio samples output)
(destroy output)
