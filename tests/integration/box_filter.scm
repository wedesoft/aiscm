(use-modules (aiscm magick) (aiscm core))
(define (box-filter img) (/ (convolve img (fill <sint> '(5 5) 1)) 25))
(write-image (to-type (rgb <ubyte>) (box-filter (read-image "star-ferry.jpg"))) "box-filter.jpg")
