(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

((llvm-typed
  (list <int>)
  (lambda (n)
    (typed-let
      [(shape   (llvmlist n))
       (strides (llvmlist (typed-constant <int> 1)))
       (ptr     (typed-call (pointer <int>) "scm_gc_malloc_pointerless" (list <int>) (list (* (size-of <int>) n))))]
       (store ptr (typed-constant <int> 2))
       (store (+ ptr 4) (typed-constant <int> 3))
       (store (+ ptr 8) (typed-constant <int> 5))
      (llvmarray ptr ptr shape strides))))
  256)

