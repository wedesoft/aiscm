(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

((llvm-typed (list <int>) (lambda (s) (typed-call (pointer <byte>) "scm_gc_malloc_pointerless" (list <int>) (list s)))) 10000000)

((llvm-typed
  (list <int>)
  (lambda (n)
    (typed-let
      [(shape   (llvmlist n))
       (strides (llvmlist (typed-constant <int> 1)))
       (ptr     (typed-call (pointer <int>) "scm_gc_malloc_pointerless" (list <int>) (list (* 4 n))))]
      (llvmarray ptr ptr shape strides))))
  256)

