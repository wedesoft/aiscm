(use-modules (aiscm magick) (aiscm convolution) (aiscm sequence) (aiscm element) (aiscm rgb) (aiscm int) (aiscm jit))
(define (box-filter img) (/ (convolve img (fill <sint> '(5 5) 1)) 25))
(write-image (to-type <ubytergb> (box-filter (read-image "star-ferry.jpg"))) "box-filter.jpg")
