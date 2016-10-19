(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm int) (aiscm obj))
(define ctx (make <context>))
(define f (jit ctx (list <int> <obj>) +))
(f 1 (/ 2 3))
; 5/3
