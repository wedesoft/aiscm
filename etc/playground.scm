(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

((llvm-typed (list <int> (complex <float>)) (lambda (x y) (llvm-if (lt x (real-part y)) x y))) 2 3)
