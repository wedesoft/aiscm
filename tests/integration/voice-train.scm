(use-modules (oop goops) (ice-9 textual-ports) (ice-9 format) (aiscm core) (aiscm ffmpeg) (aiscm pulse))
(define audio (open-ffmpeg-input "voice-commands.mp3"))
(define output (make <pulse-play> #:rate (rate audio) #:channels 1 #:typecode (typecode audio)))
(define t 0.0)
(define word "stop")
(define next-t 0.0)
(define next-word "stop")
(call-with-input-file "voice-commands.csv"
  (lambda (port)
    (get-line port)
    (while #t
      (let [(line (get-line port))]
        (if (eof-object? line) (break))
        (let [(columns (string-split line #\,))]
          (set! word next-word)
          (set! next-t (string->number (car columns)))
          (set! next-word (cadr columns))))
      (drain output)
      (format #t "current output: ~a~&" word)
      (while (< t next-t)
        (let [(samples (read-audio audio 512))]
          (if (or (not samples) (< (car (shape samples)) 512))
            (break))
          (write-audio samples output)
          (set! t (+ t (/ 512 (rate audio)))))))))
