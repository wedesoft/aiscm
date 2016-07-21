(use-modules (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm util) (aiscm element) (aiscm image))
; https://www.youtube.com/watch?v=cGgf_dbDMsw
(define video (open-ffmpeg-input "av-sync.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:type (typecode video)))
(show
  (lambda (dsp)
    (while (< (latency pulse) 0.2)
      (write-samples (read-audio video) pulse))
    (format #t "~8,2f ~8,2f~&" (- (video-pts video) (- (audio-pts video) (latency pulse))) (latency pulse))
    (synchronise (read-video video) (- (video-pts video) (- (audio-pts video) (latency pulse))) (event-loop dsp))))
(drain pulse)
