(use-modules (aiscm magick) (aiscm core) (aiscm filters))
(write-image (to-type (rgb <ubyte>) (gauss-blur (read-image "star-ferry.jpg") 2.0)) "gauss-blur.jpg")
