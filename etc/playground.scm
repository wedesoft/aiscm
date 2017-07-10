(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation))


(define a (parameter <intrgb>))
(define b (parameter <intrgb>))
(define r (parameter <intrgb>))
(define f (+ a b))
(+= r f)

(decompose-value <intrgb> f)

;(content <rgb<>> r)
;(content <rgb<>> f)
;(append-map += (content <rgb<>> r) (content <rgb<>> f))

(define ctx (make <context>))
((jit ctx (list <int> <int> <int>) +) 2 3 5)
((jit ctx (list <intrgb> <intrgb> <intrgb>) +) (rgb 2 3 5) (rgb 3 5 7) (rgb 5 7 11))

(test-begin "playground")
(test-end "playground")
