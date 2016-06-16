(use-modules (aiscm magick) (aiscm rgb) (aiscm pointer))
(define img (read-image "pavillion.jpg"))
(write-image (rgb (blue img) (green img) (red img)) "swap-channels.jpg")

