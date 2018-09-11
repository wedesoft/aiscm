(use-modules (aiscm magick) (aiscm core))
(define img (read-image "pavillion.jpg"))
(write-image (rgb (red img) (blue img) (green img)) "swap-channels.jpg")
