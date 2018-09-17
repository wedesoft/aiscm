(use-modules (aiscm core) (aiscm magick))
(write-image (dilate (read-image "star-ferry.jpg") 5) "dilated.jpg")
