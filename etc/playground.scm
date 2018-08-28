(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(define s (arr 2 3 5))

(define f (jit (list (llvmarray <ubyte> 1))
  (lambda (arr)
    (typed-let [(r (typed-alloca (typecode arr)))
                (p (typed-alloca (pointer (typecode arr))))
                (pend (+ (memory arr) (llvm-last (shape arr))))]
      (store r (fetch (memory arr)))
      (store p (+ (memory arr) 1))
      (llvm-while (ne (fetch p) pend)
        (store r (+ (fetch r) (fetch (fetch p))))
        (store p (+ (fetch p) 1)))
      (fetch r)))))

(f s)
