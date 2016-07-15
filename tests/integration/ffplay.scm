(use-modules (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm util) (aiscm element) (aiscm image))
; https://www.youtube.com/watch?v=cGgf_dbDMsw
(define video (open-ffmpeg-input "av-sync.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:type (typecode video)))
(define (play-audio video)
  (let [(frame (read-audio/video video))]
    (if (or (not frame) (is-a? frame <image>)) frame (begin (write-samples frame pulse) (play-audio video)))))
(show (lambda (dsp)
  (play-audio video)))
(drain pulse)
