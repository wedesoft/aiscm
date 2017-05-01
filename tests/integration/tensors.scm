(use-modules (oop goops) (aiscm sequence) (aiscm tensor))
(tensor 42)
;42
(tensor (seq 2 3 5))
;#<sequence<int<8,unsigned>>>:
;(2 3 5)
(tensor (+ (seq 2 3 5) 1))
;#<sequence<int<8,unsigned>>>:
;(3 4 6)
(tensor (dim i (+ (get (seq 2 3 5) i) 1)))
;#<sequence<int<8,unsigned>>>:
;(3 4 6)
(tensor i (+ (get (seq 2 3 5) i) 1))
;#<sequence<int<8,unsigned>>>:
;(3 4 6)
(tensor i j (get (arr (2 3 5) (3 5 7)) j i))
;#<sequence<sequence<int<8,unsigned>>>>:
;((2 3)
; (3 5)
; (5 7))
(tensor i j (+ (get (seq 0 1 2) i) (get (seq 10 20) j)))
;#<sequence<sequence<int<8,unsigned>>>>:
;((10 11 12)
; (20 21 22))
