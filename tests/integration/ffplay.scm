(use-modules (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm util) (aiscm element) (aiscm image))
; https://www.youtube.com/watch?v=cGgf_dbDMsw
(define video (open-ffmpeg-input "av-sync.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:type (typecode video)))
(define time (clock))
(show
  (lambda (dsp)
    (while (< (audio-pts video) (+ (video-pts video) (/ 1 15)))
      (write-samples (read-audio video) pulse))
    ;(format #t "~8,2f ~8,2f ~8,2f~&" (latency pulse) (exact->inexact (video-pts video)) (exact->inexact (audio-pts video)))
    (synchronise (read-video video) time (video-pts video) (event-loop dsp))))
(drain pulse)
