(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(llvm-typed (list (complex <float>)) (lambda (i) (llvm-if (lt (real-part i) 0) (- i) i)))
