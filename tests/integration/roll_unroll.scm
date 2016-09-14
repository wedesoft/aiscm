(use-modules (aiscm sequence) (aiscm element))
(define a (arr ((1 2 3) (4 5 6))))
a
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;((((1 2 3)
;   (4 5 6))))
(shape a)
;(3 2 1)
(roll a)
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;(((1 4))
; ((2 5))
; ((3 6)))
(shape (roll a))
;(2 1 3)
(unroll a)
;#<sequence<sequence<sequence<sequence<int<8,unsigned>>>>>>:
;(((1)
;  (2)
;  (3))
; ((4)
;  (5)
;  (6)))
(shape (unroll a))
;(1 3 2)
(project (roll a))
;#<sequence<sequence<int<8,unsigned>>>>:
;((1 4))
