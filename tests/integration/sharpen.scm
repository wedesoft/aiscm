(use-modules (aiscm magick) (aiscm convolution) (aiscm tensor) (aiscm sequence) (aiscm element) (aiscm rgb) (aiscm int) (aiscm jit))
(define (sharpen img) (tensor (max 0 (min 255 (convolve img (arr (0 -1 0) (-1 5 -1) (0 -1 0)))))))
(write-image (to-type <ubytergb> (sharpen (read-image "star-ferry.jpg"))) "sharpen.jpg")
