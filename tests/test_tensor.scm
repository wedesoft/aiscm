(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (oop goops)
             (aiscm asm)
             (aiscm jit)
             (aiscm variable)
             (aiscm expression)
             (aiscm operation)
             (aiscm loop)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm tensor))


(test-begin "aiscm tensor")

(define ctx (make <context>))

(test-begin "compiling tensors")
  (let [(s (seq <int> 2 3 5))
        (t (seq <int> 3 5 7))
        (m (arr <int> (2 3 5) (7 11 13) (17 19 23)))
        (r (arr <int> (2 3 5) (7 11 13)))
        (i (var <long>))
        (j (var <long>))]
    (test-equal "switch dimensions of a 2D tensor"
      '((2 7 17) (3 11 19) (5 13 23))
      (to-list ((jit ctx (list (class-of m))
                     (lambda (m) (indexer i (indexer j (get (get m j) i) (cadr (shape m))) (car (shape m)))))
                m)))
    (test-equal "tensor macro provides local variable"
      (to-list s) (to-list ((jit ctx (list (class-of s)) (lambda (s) (dim k (get s k)))) s)))
    (test-equal "switch dimensions of a non-square 2D tensor"
      '((2 7) (3 11) (5 13))
      (to-list ((jit ctx (list (class-of r))
                     (lambda (r) (indexer i (indexer j (get (get r j) i) (cadr (shape r))) (car (shape r)))))
                r)))
    (test-equal "tensor expression for element-wise sum"
       '(5 8 12)
       (to-list ((jit ctx (list (class-of s) (class-of t)) (lambda (s t) (dim k (+ (get s k) (get t k))))) s t))))
  (test-equal "compile and run one-dimensional identity tensor"
    '(2 3 5) (to-list ((jit ctx (list (sequence <ubyte>)) identity) (seq 2 3 5))))
  (test-equal "compile and run two-dimensional identity tensor"
    '((2 3 5) (3 5 7)) (to-list ((jit ctx (list (multiarray <ubyte> 2)) identity) (arr (2 3 5) (3 5 7)))))
  (test-equal "reconstitute a 1D tensor"
    '(2 3 5)
    (to-list ((jit ctx (list (sequence <ubyte>)) (lambda (s) (dim k (get s k))))
              (seq 2 3 5))))
  (test-equal "reconstitute a 2D tensor"
    '((2 3 5) (3 5 7))
    (to-list ((jit ctx (list (multiarray <ubyte> 2))
                       (lambda (m) (dim j (dim i (get (get m j) i)))))
              (arr (2 3 5) (3 5 7)))))
  (test-equal "transpose 2D tensor"
    '((2 3) (3 5) (5 7))
    (to-list ((jit ctx (list (multiarray <ubyte> 2))
                   (lambda (m) (dim j (dim i (+ (get (get m i) j))))))
              (arr (2 3 5) (3 5 7)))))
  (test-equal "element-wise addition of two arrays"
    '(5 8 12)
    (to-list ((jit ctx (list (sequence <ubyte>) (sequence <ubyte>)) +)
              (seq 2 3 5) (seq 3 5 7))))
  (test-equal "access one-dimensional array twice using same index in tensor operation"
    '(5 8 12)
    (to-list ((jit ctx (list (sequence <ubyte>) (sequence <ubyte>))
                       (lambda (s u) (dim k (+ (get s k) (get u k)))))
              (seq 2 3 5) (seq 3 5 7))))
  (test-equal "use array twice in tensor expression"
    '(4 6 10)
    (to-list ((jit ctx (list (sequence <ubyte>)) (lambda (s) (+ s s)))
              (seq 2 3 5))))
  (test-equal "array scalar sum in tensor expression"
    '(9 10 12)
    (to-list ((jit ctx (list (sequence <ubyte>) <int>) (lambda (s x) (+ s x)))
              (seq 2 3 5) 7)))
  (test-equal "add 2D array and transposed version of itself"
    '((4 8) (8 14))
    (to-list ((jit ctx (list (multiarray <ubyte> 2))
                   (lambda (m) (dim j (dim i (+ (get (get m j) i) (get (get m i) j))))))
              (arr (2 3) (5 7)))))
  (test-equal "tensor 1D plus with scalar"
    '(3 4 6)
    (to-list ((jit ctx (list (sequence <ubyte>) <ubyte>)
                   (lambda (a b) (dim i (+ (get a i) b))))
              (seq 2 3 5) 1)))
(test-end "compiling tensors")

(test-begin "tensor expressions")
  (let* [(s  (parameter (sequence <sint>)))
         (t  (parameter (sequence <uint>)))
         (f  (+ s t))
         (l  (loop-details (multi-loop f)))
         (ft (dim k (+ (get s k) (get t k))))
         (lt (loop-details (multi-loop ft)))]
    (test-equal "tensor sum uses loops with two typecodes"
      (list <sint> <uint>) (map typecode l))
    (test-equal "explicitly indexed tensor sum uses loops with two typecodes"
      (list <sint> <uint>) (map typecode lt)))
(test-end "tensor expressions")

(test-begin "identify tensor operations")
  (test-equal "+ is a tensor operation"
    '(#t #f #f) (tensor-operations '(+ x y)))
  (test-equal "- is a tensor operation"
    '(#t #f) (tensor-operations '(- x)))
  (test-assert "x is not a tensor operation"
    (not (tensor-operations 'x)))
  (test-assert "\"read-image\" is not a tensor operation"
    (not (tensor-operations '(read-image "test.bmp"))))
  (test-equal "\"get\" is a tensor operation"
    '(#t #f #t) (tensor-operations '(get s k)))
  (test-equal "\"get\" with two arguments is a tensor operation"
    '(#t #f #t #t) (tensor-operations '(get s i j)))
  (test-equal "\"dim\" is a tensor operation"
    '(#t #t #f) (tensor-operations '(dim k s)))
  (test-equal "\"dim\" with two arguments is a tensor operation"
    '(#t #t #t #f) (tensor-operations '(dim i j s)))
  (test-equal "\"inject\" is a tensor operation"
    '(#t #t #t #f) (tensor-operations '(inject += i s)))
(test-end "identify tensor operations")

(test-begin "convert tensor expression to identifier")
  (test-equal "filter variable names in expression"
    '_ (expression->identifier 'x))
  (test-equal "filter numeric arguments of expression"
    '_ (expression->identifier 42))
  (test-equal "preserve unary plus operation"
    '(+ _) (expression->identifier '(+ x)))
  (test-equal "preserve unary minus operation"
    '(- _) (expression->identifier '(- x)))
  (test-equal "preserve binary plus operation"
    '(+ _ _) (expression->identifier '(+ x y)))
  (test-equal "works recursively"
    '(+ (- _) _) (expression->identifier '(+ (- x) y)))
  (test-equal "filter non-tensor operations"
    '_ (expression->identifier '(read-image "test.bmp")))
  (test-equal "preverse tensor index when accessing array"
    '(get _ k) (expression->identifier '(get s k)))
  (test-equal "indexing of an expression"
    '(get (+ _) k) (expression->identifier '(get (+ s) k)))
(test-end "convert tensor expression to identifier")

(test-begin "convert identifier to symbol")
  (test-equal "simple symbol stays same"
    '_ (identifier->symbol '_))
  (test-equal "expression is converted to symbol"
    "(- _)" (symbol->string (identifier->symbol '(- _))))
(test-end "convert identifier to symbol")

(test-begin "extract variables of tensor expression")
  (test-equal "detect variable name"
    '(x) (tensor-variables 'x))
  (test-equal "detect numerical arguments"
    '(42) (tensor-variables 42))
  (test-equal "extract argument of unary plus"
    '(x) (tensor-variables '(+ x)))
  (test-equal "extract argument of unary minus"
    '(x) (tensor-variables '(- x)))
  (test-equal "extract arguments of binary plus"
    '(x y) (tensor-variables '(+ x y)))
  (test-equal "extract variables recursively"
    '(x y) (tensor-variables '(+ (- x) y)))
  (test-equal "extract non-tensor operations"
    '((read-image "test.bmp")) (tensor-variables '(read-image "test.bmp")))
  (test-equal "extract variables of tensor index access"
    '(s) (tensor-variables '(get s k)))
(test-end "extract variables of tensor expression")

(test-begin "convert tensor identifier to tensor expression")
  (test-equal "single variable expression"
    (cons 'x '(y z)) (build-expression '_ '(x y z)))
  (test-equal "reconstruct unary plus operation"
    (cons '(+ x) '(y)) (build-expression '(+ _) '(x y)))
  (test-equal "build arguments of unary expression"
    (cons '(x) '(y z)) (consume-variables '(#f) '(_) '(x y z)))
  (test-equal "build arguments of binary expression"
    (cons '(x y) '(z)) (consume-variables '(#f #f) '(_ _) '(x y z)))
  (test-equal "apply argument mask when consuming variables"
    (cons '(+ x y) '(z)) (consume-variables '(#t #f #f) '(+ _ _) '(x y z)))
  (test-equal "build binary expression"
    (cons '(+ x y) '(z)) (build-expression '(+ _ _) '(x y z)))
  (test-equal "consume variables recursively"
    (cons '((- x)) '(y z)) (consume-variables '(#f) '((- _)) '(x y z)))
  (test-equal "reconstruct single variable expression"
    'x (identifier->expression '_ '(x)))
  (test-equal "reconstruct expression"
    '(- x y) (identifier->expression '(- _ _) '(x y)))
  (test-equal "reconstruct nested expressions"
    '(+ (- x) y) (identifier->expression '(+ (- _) _) '(x y)))
  (test-equal "insert non-tensor operations"
    '(read-image "test.bmp") (identifier->expression '_ '((read-image "test.bmp"))))
  (test-equal "reconstruct tensor index access"
    '(get s k) (identifier->expression '(get _ k) '(s)))
(test-end "convert tensor identifier to tensor expression")

(test-begin "tensor macro")
  (test-equal "test trivial tensor"
    '(2 3 5) (to-list (tensor (seq 2 3 5))))
  (tensor (seq 2 3 5))
  (test-assert "tensor macro defines a generic for compiled methods"
    (defined? '_ (current-module)))
  (test-equal "trivial tensor operation is cached using a generic"
    '(2 3 5) (to-list (_ (seq 2 3 5))))
  (test-equal "cached operation uses provided values"
    '(3 5 7) (to-list (_ (seq 3 5 7))))
  (test-equal "test trivial 2D tensor"
    '((3 4 6)) (to-list (tensor (arr (3 4 6)))))
  (define num-generics (length (generic-function-methods _)))
  (tensor (seq <int> 2 3 5))
  (test-eqv "tensor operations are cached for each type"
    (1+ num-generics) (length (generic-function-methods _)))
  (test-equal "test tensor sum"
    '(5 8 12) (to-list (tensor (+ (seq 2 3 5) (seq 3 5 7)))))
  (tensor (+ (seq 2 3 5) (seq 3 5 7)))
  (test-equal "tensor sum is cached using a generic"
    '(5 8 12) (to-list ((eval (string->symbol "(+ _ _)") (current-module)) (seq 2 3 5) (seq 3 5 7))))
  (test-equal "using tensor macro with indexing"
    '(2 3 5) (to-list (tensor (dim k (get (seq 2 3 5) k)))))
  (test-equal "use tensor macro to transpose matrix"
    '((2 3) (3 5) (5 7)) (to-list (tensor (dim j (dim i (get (get (arr (2 3 5) (3 5 7)) i) j))))))
  (test-equal "specify multiple indices in one \"get\" operation"
    '((2 3 5) (3 5 7)) (to-list (tensor (dim j (dim i (get (arr (2 3 5) (3 5 7)) i j))))))
  (test-equal "specify multiple dimensions in one \"dim\" operation"
    '((2 3 5) (3 5 7)) (to-list (tensor (dim i j (get (arr (2 3 5) (3 5 7)) i j)))))
  (test-equal "specify dimension using \"tensor\" statement"
    '(2 3 5) (to-list (tensor i (get (seq 2 3 5) i))))
  (test-equal "specify multiple dimensions using\"tensor\" statement"
    '((2 3 5) (3 5 7)) (to-list (tensor i j (get (arr (2 3 5) (3 5 7)) i j))))
  (test-eqv "Trivial scalar tensor"
    42 (tensor 42))
  (test-equal "Array plus scalar operation in tensor"
    '(3 4 6) (to-list (tensor (+ (seq 2 3 5) 1))))
  (test-equal "Create sum table using tensor"
    '((10 11 12) (20 21 22)) (to-list (tensor i j (+ (get (seq 0 1 2) i) (get (seq 10 20) j)))))
(test-end "tensor macro")

(test-begin "tensor reduce")
  (let [(s (parameter (sequence <ubyte>)))
        (m (parameter (multiarray <ubyte> 2)))]
    (test-equal "\"inject\" reduces 1D array to scalar"
      <ubyte> (type (inject += i (get s i))))
    (test-equal "\"inject\" reduces 2D array to 1D"
      (sequence <ubyte>) (type (inject += i (get m i))))
    (test-eqv "Sum elements using tensor expression"
      10 (tensor (inject += k (get (seq 2 3 5) k))))
    (test-eqv "Sum different elements using tensor expression"
      12 (tensor (inject += k (get (seq 2 3 7) k))))
    (test-eq "check loop details for array of integer sums"
      <ubyte> (typecode (car (loop-details (multi-loop (inject += i (get m i)))))))
    (test-eq "preserve injection when looping over array of sums"
      <injecter> (class-of (body (multi-loop (inject += i (get m i))))))
    (test-equal "shape of array of tensor sums is one-dimensional"
      (take (shape m) 1) (shape (inject += i (get m i))))
    (test-equal "Tensor sum along one axis"
      '(4 6 8) (to-list (tensor (inject += i (get (arr (1 2 3) (3 4 5)) i)))))
    (test-eq "check loop details when other axis explicitely indexed is integer"
      <ubyte> (typecode (car (loop-details (multi-loop (dim j (inject += i (get m i j))))))))
    (test-equal "Tensor sum with other axis explicitely indexed"
      '(4 6 8) (to-list (tensor i (inject += j (get (arr (1 2 3) (3 4 5)) i j)))))
    (test-eqv "Multiply elements using tensor expression"
      30 (tensor (inject *= k (get (seq 2 3 5) k))))
    (test-eqv "Shortcut for summing elements"
      10 (tensor (sum k (get (seq 2 3 5) k))))
    (test-eqv "Shortcut for Multiplying elements"
      30 (tensor (prod k (get (seq 2 3 5) k))))
    (test-eqv "Sum of non-trivial tensor expression"
      13 (tensor (sum i (+ (get (seq 2 3 5) i) 1))))
    (test-eqv "Smallest element of three"
      2 (tensor (smallest i (get (seq 5 2 3) i))))
    (test-eqv "Largest element of three"
      5 (tensor (largest i (get (seq 2 5 3) i)))))
(test-end "tensor reduce")

(test-begin "combination of tensor features")
  (let* [(s (parameter (sequence <int>)))
         (f (- s))
         (i (var <long>))]
    (test-eq "accessing an element of a function result returns a function"
      <function> (class-of (get f i)))
    (test-equal "accessing an element of a function results reduces the number of dimensions"
      '() (shape (get f i))))
  (test-equal "indexing of array array sum"
    '(5 8 12) (to-list (tensor i (get (+ (seq 2 3 5) (seq 3 5 7)) i))))
  (test-equal "indexing of array scalar sum"
    '(3 4 6) (to-list (tensor i (get (+ (seq 2 3 5) 1) i))))
  (test-equal "sum of sums"
    13 (tensor (sum i (sum j (get (arr (1 1) (1 10)) j i)))))
(test-end "combination of tensor features")

(test-end "aiscm tensor")
