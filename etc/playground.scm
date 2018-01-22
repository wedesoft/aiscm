(use-modules (oop goops) (aiscm llvm) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define data #vu8(1 2 3 4 5 6 7 8))
(define ptr (typed-pointer (bytevector->pointer data)))
((llvm-typed (list <float>) (lambda (value) (store ptr value))) 2)
