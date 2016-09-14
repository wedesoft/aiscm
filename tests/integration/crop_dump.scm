(use-modules (aiscm magick) (aiscm sequence))
(write-image (crop 200 (dump 20 (read-image "pavillion.jpg"))) "cropped.jpg")
