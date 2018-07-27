(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (srfi srfi-1) (srfi srfi-26))

(define m (make (multiarray <int> 1) #:shape '(32)))

(
  (llvm-typed (list (llvmarray <int> 1))
    (lambda (self)
      (typed-let [(ptr (typed-alloca (pointer <int>)))]
        (store ptr (memory self))
        (llvm-while (ne (fetch ptr) (+ (memory self) (typed-constant <int> (* 10 (size-of <int>)))))
          (store (fetch ptr) (typed-constant <int> 1))
          (store ptr (+ (fetch ptr) (size-of <int>))))
        self)))
  m
)


(
  (llvm-typed (list (llvmarray <int> 1))
    (lambda (self)
      (typed-let [(mem (typed-call (pointer <int>) "scm_gc_malloc_pointerless" (list <int>) (list (* (size-of <int>) (llvm-last (shape self))))))
                  (p   (typed-alloca (pointer <int>)))
                  (q   (typed-alloca (pointer <int>)))]
      (store p mem)
      (store q (memory self))
      (llvm-while (ne (fetch p) (+ mem (* (size-of <int>) (llvm-last (shape self)))))
        (store (fetch p) (- (fetch (fetch q))))
        (store p (+ (fetch p) (size-of <int>)))
        (store q (+ (fetch q) (size-of <int>))))
      (llvmarray mem mem (shape self) (strides self)))))
  m
  )
