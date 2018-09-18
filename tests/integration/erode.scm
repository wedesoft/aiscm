(use-modules (aiscm core) (aiscm magick))
(write-image (erode (read-image "star-ferry.jpg") 5) "eroded.jpg")
