(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define data #vu8(1 2 3 4 5 6 7 8 9 10))
(define ptr  (typed-pointer (complex <float>) (bytevector->pointer data)))

(llvm-typed (list <complex<float>>) (lambda (value) (store ptr value)))
