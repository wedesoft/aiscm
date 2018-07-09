(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))


(define m (make (multiarray <int> 2) #:shape '(6 4)))

((llvm-typed (list (llvmarray <int> 2)) shape) m)

((llvm-typed (list (llvmarray <int> 2)) (lambda (m) (* (get (shape m) 1) (get (shape m) 0)))) m)
