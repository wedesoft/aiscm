;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor) (aiscm variable) (aiscm loop) (aiscm pointer) (aiscm complex))

(define ctx (make <context>))

(* 1+2i 3+4i)

((jit ctx (list (complex <int>) (complex <int>)) *) 1+2i 3+4i)

(define a (parameter (complex <int>)))
(define b (parameter (complex <int>)))
(define r (parameter (complex <int>)))

((* (complex <int>) (complex <int>)) r a b)

((+= (complex <int>) (complex <int>)) a a b)
((*= (complex <int>) (complex <int>)) a a b)


(define a (parameter (rgb <int>)))
(define b (parameter (rgb <int>)))
(define r (parameter (rgb <int>)))

((* (rgb <int>) (rgb <int>)) r a b)

((+= (rgb <int>) (rgb <int>)) a a b)
((*= (rgb <int>) (rgb <int>)) a a b)
