(use-modules (aiscm int) (aiscm rgb) (aiscm sequence) (aiscm pointer) (aiscm magick) (aiscm element))
(write-image (crop '(250 200) (dump '(27 20) (read-image "pavillion.jpg"))) "crop2d.jpg")
