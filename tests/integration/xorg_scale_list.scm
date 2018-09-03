(use-modules (aiscm magick) (aiscm xorg) (aiscm core))
(define img (read-image "fubk.png"))
(show (list (* (rgb 1 0 0) img) (* (rgb 0 1 0) img) (* (rgb 0 0 1) img)) #:shape '(160 120))

