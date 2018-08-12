(use-modules (aiscm magick) (aiscm core))
(write-image (crop 200 (dump 20 (read-image "pavillion.jpg"))) "cropped.jpg")
