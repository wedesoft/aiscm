(use-modules (aiscm core) (aiscm opencv) (aiscm magick))
(define result (connected-components (gt (green (read-image "letters.png")) 0) 8))
(define count (cdr result))
(define components (car result))
(define r (to-array (map (lambda _ (random 192)) (iota count))))
(define g (to-array (map (lambda _ (random 192)) (iota count))))
(define b (to-array (map (lambda _ (random 192)) (iota count))))
(define colors (+ (rgb r g b) 64))
(set colors 0 (rgb 0 0 0))
(write-image (warp colors components) "components.png")