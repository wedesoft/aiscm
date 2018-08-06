(use-modules (aiscm core))
(define a (arr ((1 2 3) (4 5 6))))
a
;#<multiarray<int<8,unsigned>,3>>:
;((((1 2 3)
;   (4 5 6))))
(shape a)
;(3 2 1)
(roll a)
;#<multiarray<int<8,unsigned>,3>>:
;(((1 4))
; ((2 5))
; ((3 6)))
(shape (roll a))
;(2 1 3)
(unroll a)
;#<multiarray<int<8,unsigned>,3>>:
;(((1)
;  (2)
;  (3))
; ((4)
;  (5)
;  (6)))
(shape (unroll a))
;(1 3 2)
(project (roll a))
;#<multiarray<int<8,unsigned>,2>>:
;((1 4))
