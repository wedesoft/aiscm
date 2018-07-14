(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

((llvm-typed (list <int>) (lambda (s) (typed-call (pointer <byte>) "scm_gc_malloc_pointerless" (list <int>) (list s)))) 10000000)

((llvm-typed '() (lambda () (llvmlist (typed-constant <int> 2) (typed-constant <int> 3) (typed-constant <int> 5)))))


