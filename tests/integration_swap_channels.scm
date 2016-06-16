(use-modules (aiscm magick) (aiscm rgb) (aiscm pointer))
(define img (read-image "pavillion.jpg"))
(write-image (rgb (blue img) (red img) (green img)) "swap-channels.jpg")

