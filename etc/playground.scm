;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor) (aiscm variable) (aiscm loop) (aiscm pointer) (aiscm complex) (aiscm util))

(define-jit-method rgb rgb 3)
(define-jit-method2 rgb rgb 3)

(define ctx (make <context>))

((jit ctx (list <int> <int> <int>) rgb) 2 3 5)

(define m (parameter <intrgb>))

(define r (parameter <int>))
(define g (parameter <int>))
(define b (parameter <int>))

(duplicate m (rgb r g b))
