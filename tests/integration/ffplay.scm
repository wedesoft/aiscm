(use-modules (aiscm ffmpeg) (aiscm xorg))
(define video (open-input-video "camera.avi"))
(show (lambda (dsp) (event-loop dsp (/ 1.0 (frame-rate video))) (read-video video)))
