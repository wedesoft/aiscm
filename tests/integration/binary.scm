(use-modules (aiscm sequence) (aiscm element) (aiscm pointer) (aiscm jit))
(define a (arr (1 2 3) (4 5 6)))
a
;#<sequence<sequence<int<8,unsigned>>>>:
;((1 2 3)
; (4 5 6))
(shape a)
;(3 2)
(define b (seq -1 1))
b
;#<sequence<int<8,signed>>>:
;(-1 1)
(shape b)
;(2)
(+ b 1)
;#<sequence<int<16,signed>>>:
;(0 2)
(+ b b)
;#<sequence<int<8,signed>>>:
;(-2 2)
(- 1 b)
;#<sequence<int<16,signed>>>:
;(2 0)
(* a b)
;#<sequence<sequence<int<16,signed>>>>:
;((-1 -2 -3)
; (4 5 6))
