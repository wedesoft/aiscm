(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (aiscm util)
  #:use-module (aiscm jit)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (fill duplicate)
  #:re-export (+ - *))
(define ctx (make <jit-context>))

(define-syntax-rule (element-wise (type p start n step) body ...)
  (env [(p     <long>)
        (delta <long>)
        (stop  <long>)
        (incr  <long>)]
    (MOV delta n)
    (IMUL delta step)
    (LEA stop (ptr type start delta))
    (MOV p start)
    (IMUL incr step (size-of type))
    (until (CMP p stop)
           body ...
           (ADD p incr))) )

(define-method (unary-op (r_ <pointer<>>) (a_ <pointer<>>) op)
  (env [(r (typecode r_))]
    (MOV r (ptr (typecode a_) (get-value a_)))
    (op r)
    (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (unary-op (r_ <sequence<>>) (a_ <sequence<>>) op)
  (env [(*a  <long>)
        (a+  <long>)]
    (MOV *a (get-value a_))
    (MOV a+ (last (strides a_)))
    (IMUL a+ a+ (size-of (typecode a_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (unary-op (project (rebase *r r_)) (project (rebase *a a_)) op)
                  (ADD *a a+))))

(define-method (binary-op (r_ <pointer<>>) (a_ <pointer<>>) (b_ <var>) op)
  (env [(r (typecode r_))]
    ((if (eqv? (bits (typecode r_)) (bits (typecode a_)))
       MOV
       (if (signed? (typecode a_)) MOVSX MOVZX))
     r (ptr (typecode a_) (get-value a_)))
    (if (eqv? (bits (typecode r_)) (bits (typecode b_)))
      (op r b_)
      (env [(b (typecode r_))]
        ((if (signed? (typecode b_)) MOVSX MOVZX) b b_)
        (op r b)))
    (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (r_ <pointer<>>) (a_ <var>) (b_ <pointer<>>) op)
  (env [(r (typecode r_))]
    ((if (eqv? (bits (typecode r_)) (bits (typecode a_)))
       MOV
       (if (signed? (typecode a_)) MOVSX MOVZX))
     r a_)
    (if (eqv? (bits (typecode r_)) (bits (typecode b_)))
      (op r (ptr (typecode b_) (get-value b_)))
      (env [(b (typecode r_))]
        ((if (signed? (typecode b_)) MOVSX MOVZX) b (ptr (typecode b_) (get-value b_)))
        (op r b)))
    (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (r_ <pointer<>>) (a_ <pointer<>>) (b_ <pointer<>>) op)
  (env [(r (typecode r_))]
    ((if (eqv? (bits (typecode r_)) (bits (typecode a_)))
       MOV
       (if (signed? (typecode a_)) MOVSX MOVZX))
     r (ptr (typecode a_) (get-value a_)))
    (if (eqv? (bits (typecode r_)) (bits (typecode b_)))
      (op r (ptr (typecode b_) (get-value b_)))
      (env [(b (typecode r_))]
        ((if (signed? (typecode b_)) MOVSX MOVZX) b (ptr (typecode b_) (get-value b_)))
        (op r b)))
    (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (r_ <sequence<>>) (a_ <sequence<>>) (b_ <var>) op)
  (env [(*a  <long>)
        (a+  <long>)]
    (MOV *a (get-value a_))
    (MOV a+ (last (strides a_)))
    (IMUL a+ a+ (size-of (typecode a_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (binary-op (project (rebase *r r_)) (project (rebase *a a_)) b_ op)
                  (ADD *a a+))))
(define-method (binary-op (r_ <sequence<>>) (a_ <sequence<>>) (b_ <pointer<>>) op)
  (env [(b (typecode b_))]
    (MOV b (ptr (typecode b_) (get-value b_)))
    (binary-op r_ a_ b op)))
(define-method (binary-op (r_ <sequence<>>) (a_ <var>) (b_ <sequence<>>) op)
  (env [(*b  <long>)
        (b+  <long>)]
    (MOV *b (get-value b_))
    (MOV b+ (last (strides b_)))
    (IMUL b+ b+ (size-of (typecode b_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (binary-op (project (rebase *r r_)) a_ (project (rebase *b b_)) op)
                  (ADD *b b+))))
(define-method (binary-op (r_ <sequence<>>) (a_ <pointer<>>) (b_ <sequence<>>) op)
  (env [(a (typecode a_))]
    (MOV a (ptr (typecode a_) (get-value a_)))
    (binary-op r_ a b_ op)))
(define-method (binary-op (r_ <sequence<>>) (a_ <sequence<>>)  (b_ <sequence<>>) op)
  (env [(*a  <long>)
        (a+  <long>)
        (*b  <long>)
        (b+  <long>)]
    (MOV *a (get-value a_))
    (MOV a+ (last (strides a_)))
    (MOV *b (get-value b_))
    (MOV b+ (last (strides b_)))
    (IMUL a+ a+ (size-of (typecode a_)))
    (IMUL b+ b+ (size-of (typecode b_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (binary-op (project (rebase *r r_))
                             (project (rebase *a a_))
                             (project (rebase *b b_))
                             op)
                  (ADD *a a+)
                  (ADD *b b+))))

(define-syntax-rule (define-unary-op name op)
  (define-method (name (a <element>))
    (let [(fun (wrap ctx <null> (list (class-of a) (class-of a))
                 (lambda (r_ a_) (list (unary-op r_ a_ op) (RET)))))]
      (add-method! name
                   (make <method>
                         #:specializers (list (class-of a))
                         #:procedure (lambda (a)
                                       (let [(r (make (class-of a) #:shape (shape a)))]
                                         (fun r a)
                                         r)))))
    (name a)))

(define-method (shape a b)
  (let [(shape-a (shape a))
        (shape-b (shape b))]
    (if (>= (length shape-a) (length shape-b)) shape-a shape-b)))

(define-syntax-rule (define-binary-op name op)
  (begin
    (define-method (name (a <element>) (b <element>))
      (let* [(result-type (coerce (class-of a) (class-of b)))
             (fun         (wrap ctx <null>
                                (list result-type (class-of a) (class-of b))
                                (lambda (r_ a_ b_) (list (binary-op r_ a_ b_ op) (RET)))))]
        (add-method! name
                     (make <method>
                           #:specializers (list (class-of a) (class-of b))
                           #:procedure (lambda (a b)
                                         (let [(r (make (coerce (class-of a) (class-of b)) #:shape (shape a b)))]
                                           (fun r (get a) (get b))
                                           r)))))
      (name a b))
    (define-method (name (a <element>) b) (name a (make (match b) #:value b)))
    (define-method (name a (b <element>)) (name (make (match a) #:value a) b))))

(define-unary-op duplicate (const '()))
(define-unary-op - NEG)

(define-binary-op + ADD)
(define-binary-op - SUB)
(define-binary-op * IMUL)

(define (fill type shape value); TODO: replace with tensor operation
  (let [(retval (make (multiarray type (length shape)) #:shape shape))]
    (store retval value)
    retval))
