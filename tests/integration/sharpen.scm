(use-modules (aiscm magick) (aiscm core))
(define (sharpen img) (major 0 (minor 255 (convolve img (arr (0 -1 0) (-1 5 -1) (0 -1 0))))))
(write-image (to-type (rgb <ubyte>) (sharpen (read-image "star-ferry.jpg"))) "sharpen.jpg")
