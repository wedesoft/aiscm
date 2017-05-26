(use-modules (oop goops)
             (srfi srfi-1)
             (ice-9 curried-definitions)
             (aiscm operation)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm scalar)
             (aiscm int)
             (aiscm sequence)
             (aiscm rgb)
             (aiscm jit)
             (aiscm util))

(test-begin "playground")


(define a (parameter <int>))
(define b (parameter <int>))
(define r (parameter <int>))

(+= a b)

(define out (delegate r))
(define fun (+ a b))
(code out fun)

((term fun) r)

((+= <int> <int>) a (list a b))
((+ <int> <int>) r (list a b))

(code (delegate r) (+ a b))


(test-end "playground")
