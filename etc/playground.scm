(use-modules (srfi srfi-1) (oop goops) (aiscm core) (aiscm util))

(define (elementwise-loop delegate result . args)
  "Elementwise array operation with arbitrary arity"
  (if (zero? (dimensions result))
    (store (memory result) (apply delegate args))
    (let [(start  (make-basic-block "start"))
          (for    (make-basic-block "for"))
          (body   (make-basic-block "body"))
          (finish (make-basic-block "finish"))
          (end    (make-basic-block "end"))]
      (llvm-begin
        (build-branch start)
        (position-builder-at-end start)
        (jit-let [(pend (+ (memory result) (* (llvm-car (shape result)) (llvm-car (strides result)))))]
          (build-branch for)
          (position-builder-at-end for)
          (jit-let [(p (build-phi (pointer (typecode result))))]
            (let [(q (map (lambda (arg) (if (is-a? arg <llvmarray<>>) (build-phi (pointer (typecode arg))) #f)) args))]
              (llvm-begin
                (add-incoming p start (memory result))
                (apply llvm-begin (append-map (lambda (ptr arg) (if ptr (list (add-incoming ptr start (memory arg))) '())) q args))
                (build-cond-branch (ne p pend) body end)
                (position-builder-at-end body)
                (apply elementwise-loop
                       delegate
                       (project (rebase result p))
                       (map (lambda (ptr arg) (if ptr (fetch (project (rebase arg ptr))) arg)) q args))
                (build-branch finish)
                (position-builder-at-end finish)
                (add-incoming p finish (+ p (llvm-car (strides result))))
                (apply llvm-begin
                  (append-map
                    (lambda (ptr arg) (if ptr (list (add-incoming ptr finish (+ ptr (llvm-car (strides arg))))) '()))
                    q
                    args))
                (build-branch for)
                (position-builder-at-end end)))))))))

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
