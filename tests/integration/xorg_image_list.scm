(use-modules (aiscm magick) (aiscm xorg) (aiscm rgb) (aiscm pointer) (aiscm jit) (aiscm element))
(define img (read-image "fubk.png"))
(show (list (* (rgb 1 0 0) img) (* (rgb 0 1 0) img) (* (rgb 0 0 1) img)))
