(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define m (make (multiarray <int> 1) #:shape '(32)))

(
  (llvm-typed (list (llvmarray <int> 1))
    (lambda (self)
      (typed-let [(ptr (typed-alloca (pointer <int>)))]
        (store ptr (memory self))
        (llvm-while (lt (fetch ptr) (+ (memory self) (typed-constant <int> (* 10 (size-of <int>)))))
          (store (fetch ptr) (typed-constant <int> 1))
          (store ptr (+ (fetch ptr) (size-of <int>))))
        self)))
  m
)
