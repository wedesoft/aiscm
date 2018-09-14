(use-modules (ice-9 format) (oop goops) (aiscm ffmpeg) (aiscm xorg) (aiscm pulse) (aiscm image) (aiscm util) (aiscm core))
(define video (open-ffmpeg-input "test-lrh.mp4"))
(define pulse (make <pulse-play> #:rate (rate video) #:channels (channels video) #:typecode (typecode video) #:latency 0.1))
(show
  (lambda (dsp)
    (while (< (audio-pts video) (+ (video-pts video) 0.2))
      (write-audio (or (read-audio video (/ (rate video) 10)) (break)) pulse))
    (synchronise
      (let* [(image  (to-array (read-image video)))
             (width  (car (shape image)))
             (height (cadr (shape image)))
             (left   (crop (list (/ width 2) height) image))
             (right  (dump (list (/ width 2) 0) image))]
        (rgb (red left) (green right) (blue right)))
      (- (video-pts video) (- (audio-pts video) (latency pulse))) (event-loop dsp)))
  #:fullscreen #t)
(drain pulse)
