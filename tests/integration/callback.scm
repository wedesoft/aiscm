(use-modules (oop goops) (aiscm core))
(define f (jit (list <int> <obj>) +))
(f 1 (/ 2 3))
; 5/3
