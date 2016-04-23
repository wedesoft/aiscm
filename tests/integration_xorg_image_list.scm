(use-modules (aiscm magick) (aiscm xorg) (aiscm rgb) (aiscm op) (aiscm pointer) (aiscm jit))
(define img (read-image "fubk.png"))
(show (list (* (rgb 1 0 0) img) (* (rgb 0 1 0) img) (* (rgb 0 0 1) img)))
