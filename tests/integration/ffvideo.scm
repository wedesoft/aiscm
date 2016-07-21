(use-modules (aiscm ffmpeg) (aiscm xorg) (aiscm util))
(define video (open-ffmpeg-input "av-sync.mp4"))
(define time (clock))
(show
  (lambda (dsp)
    (format #t "video pts = ~8,2f, clock = ~8,2f~&" (video-pts video) (elapsed time))
    (synchronise (read-video video) (- (video-pts video) (elapsed time)) (event-loop dsp))))
