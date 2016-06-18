(use-modules (aiscm element) (aiscm pointer) (aiscm sequence) (aiscm int))
(define a (arr <int> ((1 2 3) (4 5 6))))
(dimensions a)
;3
(size a)
;6
(size-of a)
;24
(shape a)
;(3 2 1)
(strides a)
;(1 3 6)
(get a 1 0 0)
;2
(get a 0 0)
;#<sequence<int<32,signed>>>:
;(1 2 3)
