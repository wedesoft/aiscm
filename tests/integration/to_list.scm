(use-modules (aiscm sequence) (aiscm int) (aiscm rgb))
(define l (list (list (rgb 1 2 3) (rgb 4 5 6)) (list (rgb 2 3 4) (rgb 5 6 7))))
(define a (to-array l))
a
;#<sequence<sequence<rgb<int<8,unsigned>>>>:
;(((rgb 1 2 3) (rgb 4 5 6))
; ((rgb 2 3 4) (rgb 5 6 7)))
(to-list a)
;(((rgb 1 2 3) (rgb 2 3 4)) ((rgb 4 5 6) (rgb 5 6 7)))
