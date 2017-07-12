(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit)
             (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar))


(define a (parameter <intrgb>))
(define b (parameter <intrgb>))
(define r (parameter <intrgb>))
(define f (+ a b))
(+= r f)

(define target <intrgb>)
(define intermediate <intrgb>)
(define name +=)
(define out r)
(define args (list out f))
(define intermediates (map (lambda (arg) (parameter (type arg))) args))
(append-map duplicate intermediates args)

(define args intermediates)


(decompose-value <intrgb> f)

;(content <rgb<>> r)
;(content <rgb<>> f)
;(append-map += (content <rgb<>> r) (content <rgb<>> f))

(define ctx (make <context>))
((jit ctx (list <int> <int> <int>) +) 2 3 5)
((jit ctx (list <intrgb> <intrgb> <intrgb>) +) (rgb 2 3 5) (rgb 3 5 7) (rgb 5 7 11))

(test-begin "playground")
(test-end "playground")
