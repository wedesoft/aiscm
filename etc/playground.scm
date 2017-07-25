;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor) (aiscm variable) (aiscm loop) (aiscm pointer) (aiscm complex) (aiscm util))

(define m (fill <int> '(6 4) 0))


(get m 2 2)
(set m 2 2 1)
(set m 2 2)

(define ctx (make <context>))
((jit ctx (list (sequence <int>) <int>) +) (get m 2) 3)
