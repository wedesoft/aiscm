(use-modules (oop goops) (ice-9 format) (ice-9 rdelim) (aiscm core) (aiscm pulse) (aiscm ffmpeg))

(define words (list "stop" "go" "left" "right"))
(define rate 11025)
(define chunk 512)
(define record (make <pulse-record> #:typecode <sint> #:channels 1 #:rate rate))
(format #t "offset? ")
(define n (string->number (read-line)))
(while #t
  (let [(choice (list-ref words (random (length words))))]
    (format #t "~d: ~a~&" n choice)
    (if (not (eq? (read-char) #\newline)) (break))
    (flush record)
    (read-char)
    (let* [(count     (inexact->exact (* chunk (ceiling (/ (* rate (latency record)) chunk)))))
           (samples   (read-audio record count))
           (file-name (format #f "speech-~5,'0d-~a-~6,'0d.mp3" n choice count))]
      (define output (open-ffmpeg-output file-name #:rate rate #:typecode <sint> #:channels 1 #:audio-bit-rate 80000))
      (write-audio samples output)
      (destroy output)
      (set! n (1+ n)))))
