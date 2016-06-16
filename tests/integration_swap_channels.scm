(use-modules (aiscm magick) (aiscm rgb) (aiscm pointer))
(define img (read-image "fubk.png"))
(write-image (rgb (blue img) (green img) (red img)) "swap-channels.png")

