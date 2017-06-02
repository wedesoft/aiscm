(use-modules (oop goops) (aiscm asm) (aiscm sequence) (aiscm int) (aiscm operation) (aiscm jit))

(define ctx (make <context>))

(define g (jit ctx (list (sequence <ubyte>) <ubyte>) <))

(g (seq 2 3 5) 3)
