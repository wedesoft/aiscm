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

(define-syntax-rule (d expr) (format #t "~a = ~a~&" (quote expr) expr))

(test-begin "playground")

(define ctx (make <context>))

(define-method (type (self <param>)) (typecode (delegate self)))
(define-method (type (self <indexer>)) (sequence (type (delegate self))))
(define-method (type (self <lookup>)) (type (delegate self)))
(define-method (type (self <function>))
  (apply (coercion self) (map type (delegate self))))

(define-class <tensor-loop> ()
  (typecodes #:init-keyword #:typecodes #:getter typecodes)
  (iterators #:init-keyword #:iterators #:getter iterators)
  (steps     #:init-keyword #:steps     #:getter steps    )
  (strides   #:init-keyword #:strides   #:getter strides  )
  (bases     #:init-keyword #:bases     #:getter bases    )
  (body      #:init-keyword #:body      #:getter body     ))

(define (tensor-loop expr)
  (let [(iterator (var <long>))
        (step     (var <long>))]
    (make <tensor-loop> #:typecodes (list (typecode expr))
                        #:iterators (list iterator)
                        #:steps (list step)
                        #:strides (list (stride (delegate expr)))
                        #:bases (list (value expr))
                        #:body (rebase iterator (delegate (delegate expr))))))

(define (loop-setup typecode iterator step stride base)
  (list (IMUL step (value stride) (size-of typecode)) (MOV iterator base)))

(define (loop-increment iterator step)
  (list (ADD iterator step)))

(define-method (code (a <indexer>) (b <param>))
  (let [(dest   (tensor-loop a))
        (source (tensor-loop b))]
    (append (append-map loop-setup (typecodes dest) (iterators dest) (steps dest) (strides dest) (bases dest))
            (append-map loop-setup (typecodes source) (iterators source) (steps source) (strides source) (bases source))
            (repeat (value (dimension a))
                    (code (body dest) (body source))
                    (append-map loop-increment (iterators dest) (steps dest))
                    (append-map loop-increment (iterators source) (steps source))))))

(test-begin "trivial tensor")
  (let* [(s (parameter (sequence <ubyte>)))
         (t (tensor-loop s))]
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (car (iterators t)) <var>))
    (test-eq "loop iterator is a 64 bit integer"
      <long> (typecode (car (iterators t))))
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (car (steps t)) <var>))
    (test-eq "step size is a 64 bit integer"
      <long> (typecode (car (steps t))))
    (test-eq "body of trivial tensor is a (scalar) parameter"
      <param> (class-of (body t)))
    (test-eq "body of trivial tensor is rebased on iterator"
      (car (iterators t)) (value (body t)))
    (test-eq "typecode of unsigned byte tensor is unsigned byte"
      <ubyte> (car (typecodes t)))
    (test-eq "typecode of short integer tensor is short integer"
      <sint> (car (typecodes (tensor-loop (parameter (sequence <sint>))))))
    (test-eq "stride of tensor is stride of input array"
      (stride (delegate s)) (car (strides t)))
    (test-eq "base of tensor is base pointer of input array"
      (value s) (car (bases t))))
(test-end "trivial tensor")

(test-begin "loop code")
  (let [(iterator (var <long>))
        (step     (var <long>))
        (stride   (parameter <long>))
        (base     (var <long>))]
  (test-equal "setup of array loop should define increment and initialise pointer"
    (list (IMUL step (value stride) 1) (MOV iterator base))
    (loop-setup <ubyte> iterator step stride base))
  (test-equal "setup of array loop adjust the step size according to the array type"
    (list (IMUL step (value stride) 2) (MOV iterator base))
    (loop-setup <usint> iterator step stride base))
  (test-equal "a loop increment should increment the loop iterator"
    (list (ADD iterator step))
    (loop-increment iterator step)))
(test-end "loop code")

(test-begin "tensor expressions")
  (test-equal "compile and run trivial identity tensor"
    '(2 3 5) (to-list ((jit ctx (list (sequence <ubyte>)) identity) (seq 2 3 5))))
(test-end "tensor expressions")

; TODO: return new iterators & steps, project, and rebase in one go
; TODO: create iterator and step for each combination of index and array pointer
; TODO: merge lookups when getting diagonal elements of an array
; TODO: (+ m (roll m))
; TODO: (+ m (project m))
; TODO: remove setup, increment, remove body, step for non-lookup, stride for non-lookup, iterator for non-lookup

; ------------------------------------------------------------------------------

;(define-method (lookups (self <indexer>)) (lookups self (index self)))
;(define-method (lookups (self <indexer>) (idx <var>)) (lookups (delegate self) idx))
;(define-method (lookups (self <lookup>) (idx <var>)) (if (eq? (index self) idx) (list self) (lookups (delegate self) idx)))
;(define-method (lookups (self <function>)) (append-map lookups (delegate self)))
;(define-method (lookups (self <function>) (idx <var>)) (append-map (cut lookups <> idx) (delegate self)))
;
;(define-method (loop-body (self <param>) (idx <var>)) self)
;(define-method (loop-body (self <indexer>) (idx <var>))
;  (if (eq? (index self) idx)
;      (loop-body (delegate self) idx)
;      (indexer (dimension self) (index self) (loop-body (delegate self) idx))))
;(define-method (loop-body (self <indexer>))
;  (loop-body (delegate self) (index self)))
;(define-method (loop-body (self <lookup>) (idx <var>))
;  (if (eq? (index self) idx)
;      (rebase (iterator self) (delegate self))
;      (lookup (index self) (loop-body (delegate self) idx) (stride self) (iterator self) (step self))))
;(define-method (loop-body (self <function>) (idx <var>))
;  (apply (name self) (map (cut loop-body <> idx) (delegate self))))
;(define-method (loop-body (self <function>))
;  (apply (name self) (map loop-body (delegate self))))
;
;(define-method (rebase value (self <indexer>))
;  (indexer (dimension self) (index self) (rebase value (delegate self))))
;(define-method (rebase value (self <lookup>))
;  (lookup (index self) (rebase value (delegate self)) (stride self) (iterator self) (step self)))
;(define-method (rebase value (self <param>)) (parameter (rebase value (delegate self))))
;
;(define (loop-setup lookup)
;  (list (IMUL (step lookup) (value (stride lookup)) (size-of (typecode lookup)))
;        (MOV (iterator lookup) (value lookup))))
;
;(define (loop-increment lookup)
;  (list (ADD (iterator lookup) (step lookup))))
;
;(define-method (code (a <indexer>) (b <param>))
;  (let [(candidates (delete-duplicates (append (lookups a) (lookups b))))]
;    (list (map loop-setup candidates)
;          (repeat (get (delegate (dimension a)))
;                  (append (code (loop-body a) (loop-body b))
;                          (map loop-increment candidates))))))
;
;(let* [(s    (parameter (sequence <ubyte>)))
;       (u    (parameter (sequence <usint>)))
;       (p    (parameter <sint>))
;       (m    (parameter (multiarray <ubyte> 2)))
;       (ls   (delegate s))
;       (lu   (delegate u))
;       (v    (var <long>))
;       (i    (var <long>))
;       (t1   (indexer (dimension s) i (get s i)))
;       (tsum (indexer (dimension s) i (+ (get s i) (get u i))))]
;  (test-equal "get lookup object of sequence"
;    (list ls) (lookups s))
;  (test-equal "get first lookup object of 2D array"
;    (list (delegate (delegate m))) (lookups m))
;  (test-equal "get second lookup object of 2D array"
;    (list (delegate (delegate (delegate m)))) (lookups (delegate m)))
;  (test-equal "get lookup objects of binary plus"
;    (list (delegate s) (delegate u)) (lookups (+ s u)))
;  (test-equal "get lookup based on same object when using tensor"
;    (list (delegate (delegate s))) (map delegate (lookups (tensor (dimension s) k (get s k)))))
;  (test-equal "get lookup using replaced variable"
;    (list i) (map index (lookups t1)))
;  (test-equal "get lookup based on same objects when using binary tensor"
;    (list (delegate (delegate s)) (delegate (delegate u)))
;    (map delegate (lookups tsum)))
;  (test-equal "get lookup using replaced variable"
;    (list i i) (map index (lookups tsum)))
;  (test-skip 2)
;  (test-assert "create new iterator when using index to select dimension"
;    (not (eq? (iterator ls) (iterator (get s i)))))
;  (test-assert "create new step value when using index to select dimension"
;    (not (eq? (step ls) (step (get s i)))))
;  (test-eq "typecode of sequence parameter"
;    <ubyte> (typecode s))
;  (test-eq "rebase a pointer"
;    v (value (rebase v (make (pointer <byte>) #:value (var <long>)))))
;  (test-eq "rebase parameter wrapping a pointer"
;    v (value (rebase v (parameter (make (pointer <byte>) #:value (var <long>))))))
;  (test-eq "rebase a sequence object"
;    v (value (rebase v s)))
;  (test-equal "rebase maintains sequence shape"
;    (shape s) (shape (rebase v s)))
;  (test-assert "projecting a sequence should drop a dimension"
;    (null? (shape (loop-body t1 i))))
;  (test-equal "do not drop a dimension if the specified index is a different one"
;    (shape s) (shape (loop-body s i)))
;  (test-equal "should drop the last dimension of a two-dimensional array"
;    (take (shape m) 1) (shape (loop-body m (index m))))
;  (test-equal "should drop the last dimension of a two-dimensional array"
;    (cdr (shape m)) (shape (loop-body m (index (delegate m)))))
;  (test-assert "2D array can be projected twice"
;    (is-a? (loop-body (loop-body m)) <param>))
;  (test-assert "project a one-dimensional tensor expression has a scalar result"
;    (null? (shape (loop-body tsum i))))
;  (test-equal "projecting a one-dimensional tensor should remove the lookup objects"
;    (list <param> <param>) (map class-of (delegate (loop-body tsum i))))
;  (test-assert "drop the last dimension if unspecified"
;    (null? (shape (loop-body s))))
;  (test-assert "drop the last dimension of an element-wise sum"
;    (null? (shape (loop-body (+ s u)))))
;  (test-assert "projected element-wise sum is a function"
;    (is-a? (loop-body (+ s u)) <function>))
;  (test-eq "projecting a sequence replaces the pointer with the iterator"
;    (iterator ls) (value (loop-body s)))
;  (test-eq "determine type of parameter"
;    <sint> (type p))
;  (test-eq "determine type of sequence"
;    (sequence <ubyte>) (type s))
;  (test-eq "coerce sequence and scalar"
;    (sequence <sint>) (type (+ s p)))
;  (test-eq "coerce two sequence types"
;    (sequence <usint>) (type (+ s u)))
;  (test-equal "setup of array loop should define increment and initialise pointer"
;    (list (IMUL (step ls) (value (stride ls)) 1) (MOV (iterator ls) (value ls)))
;    (loop-setup ls))
;  (test-equal "setup array loop for short integer array requires larger step size"
;    (list (IMUL (step lu) (value (stride lu)) 2) (MOV (iterator lu) (value lu)))
;    (loop-setup lu))
;  (test-equal "iterating over array should increase the pointer used for iteration"
;    (list (ADD (iterator ls) (step ls))) (loop-increment ls))
;  (test-equal "compile and run trivial identity tensor"
;    '(2 3 5) (to-list ((jit ctx (list (sequence <ubyte>)) identity) (seq 2 3 5))))
;  (test-equal "compile tensor operation with two arrays"
;    '(5 8 12)
;    (to-list ((jit ctx (list (sequence <ubyte>) (sequence <ubyte>))
;                       (lambda (s u) (tensor (dimension s) k (+ (get s k) (get u k)))))
;              (seq 2 3 5)
;              (seq 3 5 7))))
;  (test-skip 1)
;  (test-equal "access array twice using same index in tensor operation"
;    '(4 6 10)
;    (to-list ((jit ctx (list (sequence <ubyte>))
;                       (lambda (s) (tensor (dimension s) k (+ (get s k) (get s k)))))
;              (seq 2 3 5))))
;  (test-equal "use array twice in tensor operation"
;    '(4 6 10)
;    (to-list ((jit ctx (list (sequence <ubyte>)) (lambda (s) (+ s s)))
;              (seq 2 3 5)))))

(test-end "playground")

;(define-method (typecode (self <param>)) (typecode (type self)))
