(use-modules (aiscm magick) (aiscm sequence))
(define img (read-image "pavillion.jpg"))
(write-image (roll img) "rolled.jpg")
