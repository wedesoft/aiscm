(use-modules (oop goops) (aiscm core))
(define f (jit (list (rgb <int>) (multiarray <int> 1)) +))
(f (rgb 1 2 3) (arr <int> 4 5 6))
; #<sequence<rgb<int<32,signed>>>>:
; ((rgb 5 6 7) (rgb 6 7 8) (rgb 7 8 9))
