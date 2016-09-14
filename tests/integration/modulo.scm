(use-modules (aiscm magick) (aiscm rgb) (aiscm int) (aiscm sequence) (aiscm element))
(write-image (% (read-image "star-ferry.jpg") (rgb 250 200 150)) "modulo.jpg")
