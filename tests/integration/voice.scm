(use-modules (ice-9 ftw)
             (ice-9 format)
             (aiscm ffmpeg))


(ftw "speech/go"
  (lambda (filename statinfo flag)
    (if (eq? flag 'regular)
      (format #t "~a~&" filename))
    #t))
