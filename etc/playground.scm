(use-modules (srfi srfi-1) (oop goops) (aiscm core) (aiscm util))

((jit (list <int> <int>) +) 2 3)

((jit (list (llvmlist <int> 3)) identity) (list 2 3 5))

((jit (list (llvmlist <int> 3)) (lambda (lst) (get lst 1))) (list 2 3 5))

((jit (list <int> <int> <int>) (lambda (x y z) (llvmlist x y z))) 2 3 5)

((jit (list (llvmarray <ubyte> 1)) identity) (arr 2 3 5))

((jit (list (llvmarray <ubyte> 1)) shape) (arr 2 3 5))

((jit (list (llvmlist <int> 3)) (lambda (shp) (allocate-array <int> shp))) (list 2 3 5))

((jit (list <int> <int> <int>) (lambda (x y z) (allocate-array <int> (llvmlist x y z)))) 2 3 5)

((jit (list <int>) (lambda (x) (jit-let [(arr (allocate-array <int> (llvmlist x)))] arr))) 5)

((jit (list (llvmlist <int> 3) <int>) get) (list 1 2 3) 1)

((jit (list (llvmlist <int> 3)) (lambda (x) (+ (get x 0) (get x 1) (get x 2)))) (list 1 2 3))


(define f (jit (list <int>)
  (lambda (x)
    (let [(start  (make-basic-block "start"))
          (select (make-basic-block "select"))
          (skip   (make-basic-block "skip"))
          (end    (make-basic-block "end"))]
      (llvm-begin
        (build-branch start)
        (position-builder-at-end start)
        (build-cond-branch (lt x 0) select skip)
        (position-builder-at-end select)
        (jit-let [(y (- x))]
          (build-branch end)
          (position-builder-at-end skip)
          (build-branch end)
          (position-builder-at-end end)
          (jit-let [(r (build-phi <int>))]
            (add-incoming r select y)
            (add-incoming r skip x)
            r)))))))
