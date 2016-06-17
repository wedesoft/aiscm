(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm int) (aiscm rgb) (aiscm sequence) (aiscm pointer))
(define ctx (make <context>))
(define f (jit ctx (list (rgb <int>) (sequence <int>)) (lambda (x y) (+ x y))))
(f (rgb 1 2 3) (seq <int> 4 5 6))
; #<sequence<rgb<int<32,signed>>>>:
; ((rgb 5 6 7) (rgb 6 7 8) (rgb 7 8 9))
