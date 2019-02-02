(use-modules (aiscm magick) (aiscm image) (aiscm core) (aiscm filters))
(define img (to-array (convert-image (to-image (read-image "star-ferry.jpg")) 'GRAY)))
(define (norm x y) (sqrt (+ (* x x) (* y y))))
(write-image (to-type <ubyte> (norm (gauss-gradient-x img 2.0) (gauss-gradient-y img 2.0))) "gauss-gradient.jpg")
