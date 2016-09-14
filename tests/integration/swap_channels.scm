(use-modules (aiscm magick) (aiscm rgb))
(define img (read-image "pavillion.jpg"))
(write-image (rgb (red img) (blue img) (green img)) "swap-channels.jpg")
