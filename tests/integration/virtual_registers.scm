(use-modules (oop goops) (aiscm core))
(define f (jit (list <int> <int>) (lambda (x y) (+ x y))))
(f 12 34)
; 46 
