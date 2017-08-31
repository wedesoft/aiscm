(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))
(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm element) (aiscm int) (aiscm sequence) (aiscm pointer) (aiscm expression) (aiscm operation) (aiscm util))

(define ctx (make <context>))

; TODO: compiled-copy -> set3
; TODO: set array using list


(test-begin "playground")
(test-end "playground")
