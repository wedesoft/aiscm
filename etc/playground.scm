;(use-modules (srfi srfi-64))
(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (aiscm asm) (aiscm int) (aiscm rgb) (aiscm expression) (aiscm jit) (aiscm operation) (aiscm element) (aiscm sequence) (aiscm scalar) (ice-9 curried-definitions) (aiscm composite) (aiscm tensor) (aiscm variable) (aiscm loop) (aiscm pointer) (aiscm complex) (aiscm util))

; TODO: to-type = duplicate?

(to-type <int> <sint>)

(convert-type <int> <sint>)

; remove n-ary-base
