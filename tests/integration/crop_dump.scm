(use-modules (aiscm magick) (aiscm sequence) (aiscm pointer) (aiscm element))
(write-image (crop 200 (dump 20 (read-image "pavillion.jpg"))) "cropped.jpg")
