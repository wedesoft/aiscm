(use-modules (aiscm magick) (aiscm convolution) (aiscm sequence) (aiscm sequence) (aiscm image) (aiscm element) (aiscm int))
(define (to-gray img) (to-array (convert-image (to-image img) 'GRAY)))
(define (roberts-cross img)
  (/ (+ (abs (convolve img (arr (+1 0) (0 -1)))) (abs (convolve img (arr (0 +1) (-1 0))))) 2))
(write-image (to-type <ubyte> (roberts-cross (to-gray (read-image "star-ferry.jpg")))) "roberts-cross.jpg")

