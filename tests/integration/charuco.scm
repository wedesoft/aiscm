(use-modules (oop goops) (aiscm opencv) (aiscm image) (aiscm v4l2) (aiscm xorg) (aiscm core))
(define v (make <v4l2>))
(show
  (lambda _
    (let* [(img     (from-image (read-image v)))
           (markers (detect-markers img DICT_4X4_50))]
      (if (zero? (size-of (car markers)))
        img
        (let [(corners (interpolate-corners markers img 5 7 100 50))]
          (draw-corners img corners))))))
