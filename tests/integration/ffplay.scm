(use-modules (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm element) (aiscm image) (aiscm util))
; Creative commons audio-video sync test video https://www.youtube.com/watch?v=GKBKa9Za-FQ
(define video (open-ffmpeg-input "av-sync.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:typecode (typecode video) #:latency 0.1))
(show
  (lambda (dsp)
    (while (< (audio-pts video) (+ (video-pts video) 0.2)) (write-audio (or (read-audio video) (break)) pulse))
    (format #t "video pts = ~8,2f, audio-pts = ~8,2f, latency = ~8,2f~&" (video-pts video) (audio-pts video) (latency pulse))
    (synchronise (read-image video) (- (video-pts video) (- (audio-pts video) (latency pulse))) (event-loop dsp))))
(drain pulse)
