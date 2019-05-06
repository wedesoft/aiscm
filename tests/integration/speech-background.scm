(use-modules (oop goops) (ice-9 format) (aiscm core) (aiscm pulse) (aiscm ffmpeg))

(define rate 11025)
(define chunk 512)
(define seconds 300); background noise seconds
(define count (ceiling (/ (* rate seconds) chunk)))

(format #t "Recording ~a seconds of background noise~&" seconds)
(define output (open-ffmpeg-output "background.wav" #:rate rate #:typecode <sint> #:channels 1 #:audio-bit-rate 80000))
(define record (make <pulse-record> #:typecode <sint> #:channels 1 #:rate rate))
(define samples (read-audio record (* count chunk)))
(write-audio samples output)
(destroy output)
