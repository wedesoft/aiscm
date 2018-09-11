(use-modules (aiscm magick) (aiscm core))
(write-image (/ (read-image "star-ferry.jpg") (rgb 1 1 2)) "divided.jpg")
