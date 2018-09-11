(use-modules (ice-9 format) (aiscm ffmpeg) (aiscm xorg) (aiscm core) (aiscm util))
; Creative commons audio-video sync test video https://www.youtube.com/watch?v=GKBKa9Za-FQ
(define video (open-ffmpeg-input "av-sync.mp4"))
(define time (clock))
(show
  (lambda (dsp)
    (format #t "video pts = ~8,2f, clock = ~8,2f~%" (video-pts video) (elapsed time))
    (synchronise (read-image video) (- (video-pts video) (elapsed time)) (event-loop dsp))))
