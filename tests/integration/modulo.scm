(use-modules (aiscm magick) (aiscm rgb) (aiscm int) (aiscm sequence) (aiscm pointer))
(write-image (% (read-image "star-ferry.jpg") (rgb 250 200 150)) "modulo.jpg")

