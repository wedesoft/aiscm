(use-modules (aiscm magick) (aiscm rgb) (aiscm int) (aiscm sequence) (aiscm pointer))
(write-image (/ (read-image "star-ferry.jpg") (rgb 1 1 2)) "divided.jpg")
