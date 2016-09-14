(use-modules (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm element) (aiscm image) (aiscm util))
(define video (open-ffmpeg-input "http://peach.themazzone.com/durian/movies/sintel-1024-surround.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:typecode (typecode video) #:latency 0.1))
(show
  (lambda (dsp)
    (while (< (audio-pts video) (+ (video-pts video) 0.2)) (write-audio (or (read-audio video) (break)) pulse))
    (synchronise (read-image video) (- (video-pts video) (- (audio-pts video) (latency pulse))) (event-loop dsp))))
(drain pulse)
