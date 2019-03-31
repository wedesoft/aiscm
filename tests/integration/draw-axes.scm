(use-modules (oop goops) (aiscm core) (aiscm v4l2) (aiscm xorg) (aiscm opencv))
(define v (make <v4l2>))
(define c (read-camera-calibration "camera.yml"))
(show
  (lambda _
    (let* [(img     (to-array (read-image v)))
           (markers (detect-markers img DICT_4X4_50))
           (pose    (estimate-pose-single-markers (cdr markers) 0.02 (car c) (cdr c)))]
      (draw-detected-markers img markers))))
