(use-modules (aiscm magick) (aiscm convolution) (aiscm sequence) (aiscm sequence) (aiscm image) (aiscm element) (aiscm int))
(define (to-gray img) (to-array (convert-image (to-image img) 'GRAY)))
(define (abs-norm x y) (+ (abs x) (abs y)))
(define (sobel img)
  (/ (abs-norm (convolve img (arr (1 0 -1) (2 0 -2) ( 1  0 -1)))
               (convolve img (arr (1 2  1) (0 0  0) (-1 -2 -1)))) 8))
(write-image (to-type <ubyte> (sobel (to-gray (read-image "star-ferry.jpg")))) "sobel.jpg")

