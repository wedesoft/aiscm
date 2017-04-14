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
             (aiscm jit)
             (aiscm method)
             (aiscm util))

(test-begin "tensors")

(define ctx (make <context>))

(test-begin "type inference")
  (test-eq "determine type of parameter"
    <sint> (type (parameter <sint>)))
  (test-eq "determine type of sequence"
    (sequence <ubyte>) (type (parameter (sequence <ubyte>))))
  (test-eq "coerce sequence and scalar"
    (sequence <sint>) (type (+ (parameter (sequence <ubyte>)) (parameter <sint>))))
  (test-eq "coerce two sequence types"
    (sequence <usint>) (type (+ (parameter (sequence <ubyte>)) (parameter (sequence <usint>)))))
(test-end "type inference")

(test-begin "1D tensor")
  (let* [(s (parameter (sequence <ubyte>)))
         (t (tensor-loop s))
         (l (car (loop-details t)))]
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (iterator l) <var>))
    (test-eq "loop iterator is a 64 bit integer"
      <long> (typecode (iterator l)))
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (step l) <var>))
    (test-eq "step size is a 64 bit integer"
      <long> (typecode (step l)))
    (test-eq "body of trivial tensor is a (scalar) parameter"
      <param> (class-of (body t)))
    (test-eq "body of trivial tensor is rebased on iterator"
      (iterator l) (value (body t)))
    (test-eq "typecode of unsigned byte tensor is unsigned byte"
      <ubyte> (typecode l))
    (test-eq "typecode of short integer tensor is short integer"
      <sint> (typecode (car (loop-details (tensor-loop (parameter (sequence <sint>)))))))
    (test-equal "stride of tensor is stride of input array"
      (stride (delegate s)) (stride l))
    (test-equal "base of tensor is base pointer of input array"
      (value s) (base l)))
(test-end "1D tensor")

(test-begin "2D tensor")
  (let* [(m (parameter (multiarray <ubyte> 2)))
         (i (var <long>))
         (j (var <long>))
         (t (tensor j (tensor i (+ (get (get m i) j)))))]
    (test-assert "tensor loop preserves inner index"
      (is-a? (body (tensor-loop m)) <indexer>))
    (test-eq "inner index is second index of 2D array"
      (index (delegate m)) (index (body (tensor-loop m))))
    (test-eq "inner dimension is second dimension of 2D array"
      (dimension (delegate m)) (dimension (body (tensor-loop m))))
    (test-assert "preserve loop details when skipping indices"
      (is-a? (car (loop-details (tensor-loop m))) <loop-detail>))
    (test-assert "body of 2D tensor drops inner lookup"
      (is-a? (delegate (body (tensor-loop m))) <lookup>))
    (test-assert "tensor loop should preserve 2nd index of transposed array"
      (is-a? (delegate (body (tensor-loop t))) <lookup>)))
(test-end "2D tensor")

(test-begin "tensor expressions")
  (let* [(s  (parameter (sequence <sint>)))
         (t  (parameter (sequence <uint>)))
         (f  (+ s t))
         (l  (loop-details (tensor-loop f)))
         (ft (tensor k (+ (get s k) (get t k))))
         (lt (loop-details (tensor-loop ft)))]
    (test-equal "tensor sum uses loops with two typecodes"
      (list <sint> <uint>) (map typecode l))
    (test-equal "explicitly indexed tensor sum uses loops with two typecodes"
      (list <sint> <uint>) (map typecode lt)))
(test-end "tensor expressions")

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
    (to-list ((jit ctx (list (sequence <ubyte>)) (lambda (s) (tensor k (get s k))))
              (seq 2 3 5))))
  (test-equal "reconstitute a 2D tensor"
    '((2 3 5) (3 5 7))
    (to-list ((jit ctx (list (multiarray <ubyte> 2))
                       (lambda (m) (tensor j (tensor i (get (get m j) i)))))
              (arr (2 3 5) (3 5 7)))))
  (test-equal "transpose 2D tensor"
    '((2 3) (3 5) (5 7))
    (to-list ((jit ctx (list (multiarray <ubyte> 2))
                   (lambda (m) (tensor j (tensor i (+ (get (get m i) j))))))
              (arr (2 3 5) (3 5 7)))))
  (test-equal "element-wise addition of two arrays"
    '(5 8 12)
    (to-list ((jit ctx (list (sequence <ubyte>) (sequence <ubyte>)) +)
              (seq 2 3 5) (seq 3 5 7))))
  (test-equal "access one-dimensional array twice using same index in tensor operation"
    '(5 8 12)
    (to-list ((jit ctx (list (sequence <ubyte>) (sequence <ubyte>))
                       (lambda (s u) (tensor k (+ (get s k) (get u k)))))
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
                   (lambda (m) (tensor j (tensor i (+ (get (get m j) i) (get (get m i) j))))))
              (arr (2 3) (5 7)))))
(test-end "tensor expressions")
(test-end "tensors")
