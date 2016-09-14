(use-modules (aiscm magick) (aiscm sequence))
(write-image (crop '(250 200) (dump '(27 20) (read-image "pavillion.jpg"))) "crop2d.jpg")
