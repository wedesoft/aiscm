(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))


(define m (make (multiarray <byte> 1) #:shape '(6) #:memory (bytevector->pointer #vu8(2 3 5 7 11 13))))

((llvm-typed (list (llvmarray <sint> 1)) (lambda (m) (fetch (memory m)))) m)

