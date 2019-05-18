(use-modules (aiscm magick) (aiscm image) (aiscm core) (aiscm filters))
(define img (from-image (convert-image (to-image (read-image "star-ferry.jpg")) 'GRAY)))
(define result (harris-stephens img 1.0 0.05))
(write-image (to-type <ubyte> (major (minor (+ (/ result 1000) 127) 255) 0)) "harris-stephens.jpg")
