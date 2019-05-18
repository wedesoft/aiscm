(use-modules (oop goops) (aiscm core) (aiscm image) (aiscm v4l2) (aiscm xorg) (aiscm opencv))
(define v (make <v4l2>))
(define c (read-camera-calibration "camera.yml"))
(show
  (lambda _
    (let* [(img     (from-image (read-image v)))
           (markers (detect-markers img DICT_4X4_50))
           (pose    (estimate-pose-single-markers (cdr markers) 0.02 (car c) (cdr c)))
           (idx     (list-index (to-list (car markers)) 0))]
      (if idx (draw-axis img (car c) (cdr c) (get (car pose) idx) (get (cdr pose) idx) 0.02))
      img)))
