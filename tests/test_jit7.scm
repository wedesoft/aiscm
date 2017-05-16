(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (system foreign)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm mem)
             (aiscm pointer)
             (aiscm rgb)
             (aiscm complex)
             (aiscm obj)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm jit)
             (aiscm expression)
             (aiscm tensor)
             (aiscm method)
             (aiscm util))

(test-begin "tensors")

(define ctx (make <context>))

(test-begin "list operations")
  (test-assert "+ is an operation"
    (memv '+ operations))
  (test-assert "- is an operation"
    (memv '- operations))
  (test-assert "* is an operation"
    (memv '* operations))
(test-end "list operations")

(test-begin "loop code")
  (let* [(iterator   (var <long>))
         (step       (var <long>))
         (stride     (parameter <long>))
         (base       (var <long>))
         (loop-ubyte (make <loop-detail> #:typecode <ubyte> #:iterator iterator #:step step #:stride stride #:base base))
         (loop-usint (make <loop-detail> #:typecode <usint> #:iterator iterator #:step step #:stride stride #:base base))]
  (test-equal "setup of array loop should define increment and initialise pointer"
    (list (IMUL step (value stride) 1) (MOV iterator base))
    (loop-setup loop-ubyte))
  (test-equal "setup of array loop adjust the step size according to the array type"
    (list (IMUL step (value stride) 2) (MOV iterator base))
    (loop-setup loop-usint))
  (test-equal "a loop increment should increment the loop iterator"
    (list (ADD iterator step))
    (loop-increment loop-ubyte)))
(test-end "loop code")

(test-begin "tensor expressions")
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
(test-end "tensor expressions")

(test-begin "tensor reductions")
(test-end "tensor reductions")
(test-end "tensors")
