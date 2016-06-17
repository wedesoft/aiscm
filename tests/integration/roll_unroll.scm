(use-modules (aiscm sequence) (aiscm pointer))
(define a (arr ((1 2 3) (4 5 6))))
a
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;((((1 2 3)
;   (4 5 6))))
(roll a)
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;(((1 4))
; ((2 5))
; ((3 6)))
(unroll a)
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;(((1)
;  (2)
;  (3))
; ((4)
;  (5)
;  (6)))
