(use-modules (oop goops)
             (srfi srfi-18)
             (ice-9 readline)
             (ice-9 format)
             (ice-9 threads)
             (aiscm core)
             (aiscm ffmpeg)
             (aiscm pulse)
             (aiscm util))

(define words (list "stop" "go" "left" "right"))
(define output (open-ffmpeg-output "voice-commands.mp3" #:rate 11025 #:typecode <sint> #:channels 1 #:audio-bit-rate 80000))
(define record (make <pulse-record> #:typecode <sint> #:channels 1 #:rate 11025))
(define time (clock))
(define q #f)
(define thread
  (call-with-new-thread
    (lambda ()
      (while (not q)
        (let [(samples (read-audio record 4410))]
          (write-audio samples output))))))
(call-with-output-file "voice-commands.csv"
  (lambda (port)
    (format port "time,word~&")
    (while #t
      (let [(choice (list-ref words (random (length words))))]
        (format #t "Say \"~a\" and press enter (x and enter to exit)." choice)
        (if (not (equal? (readline) ""))
          (break))
        (format port "~a,~a~&" (elapsed time) choice)))))
(set! q #t)
(join-thread thread)
(destroy output)
