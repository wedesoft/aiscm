(use-modules (oop goops) (aiscm core))
(define f (jit (list (rgb <int>) (llvmarray <int> 1)) +))
(f (rgb 1 2 3) (arr <int> 4 5 6))
; #<multiarray<int<32,signed>,1>>:
; ((rgb 5 6 7) (rgb 6 7 8) (rgb 7 8 9))
