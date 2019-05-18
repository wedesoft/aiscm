(use-modules (aiscm magick) (aiscm xorg) (aiscm image) (aiscm core))
(define (to-gray arr) (from-image (convert-image (to-image arr) 'GRAY)))
(show (to-gray (read-image "fubk.png")))
