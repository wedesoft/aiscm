;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor))

; TODO: intermediates for +=

(define r (parameter <intrgb>))
(define a (parameter <intrgb>))
(define b (parameter <intrgb>))

(+= r a)

(+= r (+ a b))
