(use-modules (aiscm magick) (aiscm core))
(write-image (crop '(200 250) (dump '(20 27) (read-image "pavillion.jpg"))) "crop2d.jpg")
