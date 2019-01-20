(use-modules (ice-9 ftw)
             (ice-9 format)
             (aiscm core)
             (aiscm ffmpeg))


(define samples '())
(ftw "speech/go"
  (lambda (filename statinfo flag)
    (if (eq? flag 'regular) (set! samples (cons (read-audio (open-ffmpeg-input filename) 64000) samples)))
    #t))
