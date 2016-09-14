(use-modules (aiscm magick) (aiscm rgb) (aiscm sequence) (aiscm jit))
(write-image (/ (read-image "star-ferry.jpg") (rgb 1 1 2)) "divided.jpg")
