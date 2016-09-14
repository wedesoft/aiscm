(use-modules (aiscm magick) (aiscm rgb) (aiscm sequence) (aiscm element))
(define img (read-image "pavillion.jpg"))
(write-image (roll img) "rolled.jpg")
