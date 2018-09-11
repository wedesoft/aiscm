(use-modules (aiscm magick) (aiscm core))
(write-image (% (read-image "star-ferry.jpg") (rgb 250 200 150)) "modulo.jpg")
