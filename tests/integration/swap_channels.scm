(use-modules (aiscm magick) (aiscm rgb) (aiscm sequence) (aiscm element))
(define img (read-image "pavillion.jpg"))
(write-image (rgb (red img) (blue img) (green img)) "swap-channels.jpg")
