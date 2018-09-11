(use-modules (aiscm core))
(define a (arr <int> ((1 2 3) (4 5 6))))
(dimensions a)
;3
(size-of a)
;24
(shape a)
;(3 2 1)
(strides a)
;(4 12 24)
(get a 1 0 0)
;2
(get a 0 0)
;#<multiarray<int<32,signed>,1>>:
;(1 2 3)
