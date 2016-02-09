(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm int))
(define ctx (make <context>))
(define f (jit ctx (list <int> <int>) (lambda (x y) (+ x y))))
(f 12 34)
; 46 
