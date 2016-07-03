(use-modules (aiscm ffmpeg) (aiscm xorg) (aiscm util))
(define video (open-input-video "camera.avi"))
(define t (clock))
(show
  (lambda (dsp)
    (let [(img (read-video video))]
      (event-loop dsp (max 0 (- (video-pts video) (elapsed t))))
      img)))
