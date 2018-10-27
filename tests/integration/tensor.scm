(use-modules (aiscm core) (aiscm tensors))
(define a (arr (2 3 5) (3 5 7)))
(define b (arr 2 3 5))
(define-tensor (transpose a) (tensor j (tensor i (get (get a i) j))))
(transpose a)
;#<multiarray<int<8,unsigned>,2>>:
;((2 3)
; (3 5)
; (5 7))
(define-tensor (add-rows a) (sum-over i (get a i)))
(add-rows a)
;#<multiarray<int<8,unsigned>,1>>:
;(5 8 12)
(define-tensor (add-columns a) (tensor j (sum-over i (get (get a j) i))))
(add-columns a)
;#<multiarray<int<8,unsigned>,1>>:
;(10 15)
(define-tensor (x w h) (tensor (j h) (tensor (i w) i)))
(x 3 2)
;#<multiarray<int<32,signed>,2>>:
;((0 1 2)
; (0 1 2))
(define-tensor (y w h) (tensor (j h) (tensor (i w) j)))
(y 3 2)
;#<multiarray<int<32,signed>,2>>:
;((0 0 0)
; (1 1 1))
(define-tensor (dot a b) (tensor j (sum-over k (* (get (get a j) k) (get b k)))))
(dot a b)
;#<multiarray<int<8,unsigned>,1>>:
;(38 56)
(define-tensor (prod a) (product-over i (get a i)))
(prod (arr 2 3 5))
;30
