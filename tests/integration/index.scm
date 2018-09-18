(use-modules (oop goops) (aiscm core))
(define idx (index 3 2))
idx
;#<multiarray<int<32,signed>,2>>:
;((0 1 2
; (3 4 5)))
(/ idx 3)
;#<multiarray<int<32,signed>,2>>:
;((0 0 0
; (1 1 1)))
(% idx 3)
;#<multiarray<int<32,signed>,2>>:
;((0 1 2
; (0 1 2)))
