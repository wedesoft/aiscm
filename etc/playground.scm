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


(define s (parameter (sequence <ubyte>)))
(define t (parameter (sequence <ubyte>)))
(define m (parameter (multiarray <ubyte> 2)))
(define f (+ s t))
(define g (tensor (dimension s) k (+ (get s k) (get t k))))

(define-method (type (self <param>)) (typecode (delegate self)))
(define-method (type (self <indexer>)) (sequence (type (delegate self))))
(define-method (type (self <lookup>)) (type (delegate self)))
(define-method (type (self <function>))
  (apply (coercion self) (map type (delegate self))))

(define-method (typecode (self <param>)) (typecode (type self)))

(define-method (lookups (self <indexer>)) (lookups self (index self)))
(define-method (lookups (self <indexer>) (idx <var>)) (lookups (delegate self) idx))
(define-method (lookups (self <lookup>) (idx <var>)) (if (eq? (index self) idx) (list self) (lookups (delegate self) idx)))
(define-method (lookups (self <function>)) (append-map lookups (delegate self)))
(define-method (lookups (self <function>) (idx <var>)) (append-map (cut lookups <> idx) (delegate self)))

(define-method (project (self <param>) (idx <var>)) self)
(define-method (project (self <indexer>) (idx <var>))
  (if (eq? (index self) idx)
      (project (delegate self) idx)
      (indexer (dimension self) (index self) (project (delegate self) idx))))
(define-method (project (self <indexer>))
  (project self (index self)))
(define-method (project (self <lookup>) (idx <var>))
  (if (eq? (index self) idx)
      (rebase (iterator self) (delegate self))
      (lookup (index self) (project (delegate self) idx) (stride self) (iterator self) (step self))))
(define-method (project (self <function>) (idx <var>))
  (apply (name self) (map (cut project <> idx) (delegate self))))
; TODO: determine type of function result instead of storing it
(define-method (project (self <function>))
  (apply + (map project (delegate self))))

(define-method (rebase value (self <indexer>))
  (indexer (dimension self) (index self) (rebase value (delegate self))))
(define-method (rebase value (self <lookup>))
  (lookup (index self) (rebase value (delegate self)) (stride self) (iterator self) (step self)))
(define-method (rebase value (self <lookup>))
  (rebase value (delegate self)))

(define-method (setup (self <lookup>))
  (list (IMUL (step self) (get (delegate (stride self))) (size-of (typecode self)))
        (MOV (iterator self) (value self))))

(define-method (increment (self <lookup>))
  (list (ADD (iterator self) (step self))))

(let* [(s    (parameter (sequence <ubyte>)))
       (u    (parameter (sequence <ubyte>)))
       (p    (parameter <sint>))
       (m    (parameter (multiarray <ubyte> 2)))
       (v    (var <long>))
       (i    (var <long>))
       (t1   (indexer (dimension s) i (get s i)))
       (tsum (indexer (dimension s) i (+ (get s i) (get u i))))]
  (test-equal "get lookup object of sequence"
    (list (delegate s)) (lookups s))
  (test-equal "get first lookup object of 2D array"
    (list (delegate (delegate m))) (lookups m))
  (test-equal "get second lookup object of 2D array"
    (list (delegate (delegate (delegate m)))) (lookups (delegate m)))
  (test-equal "get lookup objects of binary plus"
    (list (delegate s) (delegate u)) (lookups (+ s u)))
  (test-equal "get lookup based on same object when using tensor"
    (list (delegate (delegate s))) (map delegate (lookups (tensor (dimension s) k (get s k)))))
  (test-equal "get lookup using replaced variable"
    (list i) (map index (lookups t1)))
  (test-equal "get lookup based on same objects when using binary tensor"
    (list (delegate (delegate s)) (delegate (delegate u)))
    (map delegate (lookups tsum)))
  (test-equal "get lookup using replaced variable"
    (list i i) (map index (lookups tsum)))
  (test-eq "typecode of sequence parameter"
    <ubyte> (typecode s))
  (test-equal "set up an iterator"
    (list (IMUL (step s) (get (delegate (stride s))) (size-of (typecode s))) (MOV (iterator s) (value s)))
    (setup (delegate s)))
  (test-equal "advance an iterator"
    (list (ADD (iterator s) (step s))) (increment (delegate s)))
  (test-eq "rebase a pointer"
    v (value (rebase v (make (pointer <byte>) #:value (var <long>)))))
  (test-eq "rebase parameter wrapping a pointer"
    v (value (rebase v (parameter (make (pointer <byte>) #:value (var <long>))))))
  (test-eq "rebase a sequence object"
    v (value (rebase v s)))
  (test-equal "rebase maintains sequence shape"
    (shape s) (shape (rebase v s)))
  (test-assert "projecting a sequence should drop a dimension"
    (null? (shape (project t1 i))))
  (test-equal "do not drop a dimension if the specified index is a different one"
    (shape s) (shape (project s i)))
  (test-equal "should drop the last dimension of a two-dimensional array"
    (take (shape m) 1) (shape (project m (index m))))
  (test-equal "should drop the last dimension of a two-dimensional array"
    (cdr (shape m)) (shape (project m (index (delegate m)))))
  (test-assert "project a one-dimensional tensor expression has a scalar result"
    (null? (shape (project tsum i))))
  (test-equal "projecting a one-dimensional tensor should remove the lookup objects"
    (list <param> <param>) (map class-of (delegate (project tsum i))))
  (test-assert "drop the last dimension if unspecified"
    (null? (shape (project s))))
  (test-assert "drop the last dimension of an element-wise sum"
    (null? (shape (project (+ s u)))))
  (test-assert "projected element-wise sum is a function"
    (is-a? (project (+ s u)) <function>))
  (test-eq "projecting a sequence replaces the pointer with the iterator"
    (iterator s) (value (project s)))
  (test-eq "determine type of parameter"
    <sint> (type p))
  (test-eq "determine type of sequence"
    (sequence <ubyte>) (type s))
  (test-eq "coerce two sequence types"
    (sequence <ubyte>) (type (+ s u)))
  (test-eq "coerce sequence and scalar"
    (sequence <sint>) (type (+ s p))))

; TODO: project and rebase with indexed lookup in one go
; TODO: body/project, rebase, for tensor expressions
; TODO: merge lookups when getting diagonal elements of an array

; (jit ctx (list (sequence <ubyte>) (sequence <ubyte>)) (lambda (s u) (tensor (dimension s) k (+ (get s k) (get u k)))))

(test-end "playground")
