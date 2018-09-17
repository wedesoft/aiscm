(use-modules (oop goops) (aiscm core))
(define a (fill <int> '(4 4) 0))
(set a 3 3 1)
(set a '(1 . 3) '(1 . 2)  '((2 3)))
(set a '(1 . 3) '(2 . 3) (arr (4 5)))
(set a '(1 . 3) '(3 . 4) 6)
a
;#<multiarray<int<32,signed>,2>>:
;((0 0 0 0)
; (0 2 3 0)
; (0 4 5 0)
; (0 6 6 1))
