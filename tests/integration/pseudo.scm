(use-modules (oop goops) (aiscm magick) (aiscm core) (aiscm image))
(define colors (to-array (map (lambda (i) (rgb i (- 255 i) 0)) (iota 256))))
(define img (read-image "star-ferry.jpg"))
(write-image (warp colors (to-array (convert-image (to-image img) 'GRAY))) "pseudo.jpg")
