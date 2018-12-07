(use-modules (aiscm core))
(define a (arr (1 2 3) (4 5 6)))
a
;#<multiarray<int<8,unsigned>,2>>:
;((1 2 3)
; (4 5 6))
(shape a)
;(2 3)
(define b (arr -1 1))
b
;#<multiarray<int<8,signed>,1>>:
;(-1 1)
(shape b)
;(2)
(+ b 1)
;#<multiarray<int<16,signed>,1>>:
;(0 2)
(+ b b)
;#<multiarray<int<8,signed>,1>>:
;(-2 2)
(- 1 b)
;#<multiarray<int<16,signed>,1>>:
;(2 0)
(* a b)
;#<multiarray<int<16,signed>,2>>:
;((-1 -2 -3)
; (4 5 6))
