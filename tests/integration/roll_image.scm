(use-modules (aiscm magick) (aiscm core))
(define img (read-image "pavillion.jpg"))
(write-image (roll img) "rolled.jpg")
