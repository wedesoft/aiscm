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

(define a (parameter (sequence <int>)))
(define b (parameter <int>))
(define r (parameter (sequence <int>)))

(code r (+ a b))

(code r (dim i (+ (get a i) b)))

(code r (dim i (get (+ a b) i)))

(test-end "playground")
