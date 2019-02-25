(use-modules (oop goops) (aiscm core) (aiscm v4l2) (aiscm xorg) (aiscm opencv))
(define v (make <v4l2>))
(define object-points '())
(define image-points '())
(show
  (lambda _
    (let* [(img     (to-array (read-image v)))
           (markers (detect-markers img DICT_4X4_50))]
      (if (zero? (size-of (car markers)))
        img
        (let* [(corners (interpolate-corners markers img 5 7 100 50))
               (object  (grid (1- 7) 0.02 (car corners)))
               (image   (cdr corners))]
          (if (>= (car (shape (car corners))) 5)
            (begin
              (set! object-points (cons object object-points))
              (set! image-points (cons image image-points))))
          (draw-corners img corners))))))
(define cal (camera-calibration object-points image-points (shape v)))
(format #t "error = ~a~&" (car cal))
(format #t "camera matrix = ~a~&" (cadr cal))
(format #t "distortion coefficients = ~a~&" (to-list (caddr cal)))
