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
  #:export (fill)
  #:re-export (+ -))
(define ctx (make <jit-context>))

(define-method (unary-op (fun <jit-function>) (r_ <element>) (a_ <element>) op)
  (env fun
       [(r (reg (get-value r_) fun))
        (a (reg (get-value a_) fun))]
         (MOV r a)
         (op r)))
(define-method (unary-op (fun <jit-function>) (r_ <pointer<>>) (a_ <pointer<>>) op)
  (env fun
       [(r (reg (typecode r_) fun))]
         (MOV r (ptr (typecode a_) (get-value a_)))
         (op r)
         (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (unary-op (fun <jit-function>) (r_ <sequence<>>) (a_ <sequence<>>) op)
  (env fun
       [(r+  (reg (last (strides r_)) fun))
        (a+  (reg (last (strides a_)) fun))
        (n   (reg (last (shape r_)) fun))
        (*p  (reg <long> fun))
        (*q  (reg <long> fun))
        (*rx (reg <long> fun))]
       (IMUL n r+)
       (MOV *p (get-value r_))
       (MOV *q (get-value a_))
       (LEA *rx (ptr (typecode r_) *p n))
       (IMUL r+ r+ (size-of (typecode r_)))
       (IMUL a+ a+ (size-of (typecode a_)))
       (CMP *p *rx)
       (JE 'return)
       'loop
       (unary-op fun (project (rebase *p r_)) (project (rebase *q a_)) op)
       (ADD *p r+)
       (ADD *q a+)
       (CMP *p *rx)
       (JNE 'loop)
       'return))

(define-method (binary-op (fun <jit-function>) (r_ <element>) (a_ <element>) (b_ <element>) op)
  (env fun
       [(r (reg (get-value r_) fun))
        (a (reg (get-value a_) fun))
        (b (reg (get-value b_) fun))
        (w (reg (class-of r_) fun))]
       ((if (eqv? (size-of (class-of a_)) (size-of (class-of r_)))
          MOV
          (if (signed? (class-of a_)) MOVSX MOVZX)) r a)
       (if (eqv? (size-of (class-of b_)) (size-of (class-of r_)))
         (op r b)
         (append
           ((if (signed? (class-of b_)) MOVSX MOVZX) w b)
           (op r w)))))
(define-method (binary-op (fun <jit-function>) (r_ <pointer<>>) (a_ <pointer<>>) (b_ <element>) op)
  (env fun
       [(r (reg (typecode r_) fun))
        (w (reg (typecode r_) fun))
        (b (reg (get-value b_) fun))]
       ((if (eqv? (size-of (typecode a_)) (size-of (typecode r_)))
          MOV
          (if (signed? (typecode a_)) MOVSX MOVZX)) r (ptr (typecode a_) (get-value a_)))
       (if (eqv? (size-of (class-of b_)) (size-of (typecode r_)))
         (op r b)
         (append
           ((if (signed? (class-of b_)) MOVSX MOVZX) w b)
           (op r w)))
       (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (fun <jit-function>) (r_ <pointer<>>) (a_ <element>) (b_ <pointer<>>) op)
   (env fun
       [(r (reg (typecode r_) fun))
        (w (reg (typecode r_) fun))
        (a (reg (get-value a_) fun))
        (*b (reg (get-value b_) fun))]
       ((if (eqv? (size-of (class-of a_)) (size-of (typecode r_)))
          MOV
          (if (signed? (class-of a_)) MOVSX MOVZX)) r a)
       (if (eqv? (size-of (typecode b_)) (size-of (typecode r_)))
         (op r (ptr (typecode b_) *b))
         (append
           ((if (signed? (typecode b_)) MOVSX MOVZX) w (ptr (typecode b_) *b))
           (op r w)))
       (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (fun <jit-function>) (r_ <pointer<>>) (a_ <pointer<>>) (b_ <pointer<>>) op)
  (env fun
       [(r (reg (typecode r_) fun))
        (w (reg (typecode r_) fun))]
       ((if (eqv? (size-of (typecode a_)) (size-of (typecode r_)))
          MOV
          (if (signed? (typecode a_)) MOVSX MOVZX)) r (ptr (typecode a_) (get-value a_)))
       (if (eqv? (size-of (typecode b_)) (size-of (typecode r_)))
         (op r (ptr (typecode b_) (get-value b_)))
         (append
           ((if (signed? (typecode b_)) MOVSX MOVZX) w (ptr (typecode b_) (get-value b_)))
           (op r w)))
       (MOV (ptr (typecode r_) (get-value r_)) r)))
(define-method (binary-op (fun <jit-function>) (r_ <sequence<>>) (a_ <sequence<>>) (b_ <element>) op)
  (env fun
       [(r+  (reg (last (strides r_)) fun))
        (a+  (reg (last (strides a_)) fun))
        (n   (reg (last (shape r_)) fun))
        (*p  (reg <long> fun))
        (*q  (reg <long> fun))
        (*rx (reg <long> fun))]
       (IMUL n r+)
       (MOV *p (get-value r_))
       (MOV *q (get-value a_))
       (LEA *rx (ptr (typecode r_) *p n))
       (IMUL r+ r+ (size-of (typecode r_)))
       (IMUL a+ a+ (size-of (typecode a_)))
       (CMP *p *rx)
       (JE 'return)
       'loop
       (binary-op fun (project (rebase *p r_)) (project (rebase *q a_)) b_ op)
       (ADD *p r+)
       (ADD *q a+)
       (CMP *p *rx)
       (JNE 'loop)
       'return))
(define-method (binary-op (fun <jit-function>) (r_ <sequence<>>) (a_ <element>) (b_ <sequence<>>) op)
  (env fun
       [(r+  (reg (last (strides r_)) fun))
        (a   (reg (get-value a_) fun))
        (*b  (reg (get-value b_) fun))
        (b+  (reg (last (strides b_)) fun))
        (n   (reg (last (shape r_)) fun))
        (*p  (reg <long> fun))
        (*q  (reg <long> fun))
        (*rx (reg <long> fun))]
       (IMUL n r+)
       (MOV *p (get-value r_))
       (MOV *q *b)
       (LEA *rx (ptr (typecode r_) *p n))
       (IMUL r+ r+ (size-of (typecode r_)))
       (IMUL b+ b+ (size-of (typecode b_)))
       (CMP *p *rx)
       (JE 'return)
       'loop
       (binary-op fun (project (rebase *p r_)) a_ (project (rebase *q b_)) op)
       (ADD *p r+)
       (ADD *q b+)
       (CMP *p *rx)
       (JNE 'loop)
       'return))
(define-method (binary-op (fun <jit-function>) (r_ <sequence<>>) (a_ <sequence<>>) (b_ <sequence<>>) op)
  (env fun
       [(*r  (reg (get-value r_) fun))
        (r+  (reg (last (strides r_)) fun))
        (*a  (reg (get-value a_) fun))
        (a+  (reg (last (strides a_)) fun))
        (*b  (reg (get-value b_) fun))
        (b+  (reg (last (strides b_)) fun))
        (n   (reg (last (shape r_)) fun))
        (*p  (reg <long> fun))
        (*q  (reg <long> fun))
        (*s  (reg <long> fun))
        (*rx (reg <long> fun))]
       (IMUL n r+)
       (MOV *p *r)
       (MOV *q *a)
       (MOV *s *b); TODO: (get-value b_) does not work here
       (LEA *rx (ptr (typecode r_) *p n))
       (IMUL r+ r+ (size-of (typecode r_)))
       (IMUL a+ a+ (size-of (typecode a_)))
       (IMUL b+ b+ (size-of (typecode b_)))
       (CMP *p *rx)
       (JE 'return)
       'loop
       (binary-op fun (project (rebase *p r_)) (project (rebase *q a_)) (project (rebase *s b_)) op)
       (ADD *p r+)
       (ADD *q a+)
       (ADD *s b+)
       (CMP *p *rx)
       (JNE 'loop)
       'return))

(define-method (+ (a <element>)) a)
(define-method (+ (a <element>) (b <element>))
  (add-method! + (jit-wrap ctx
                           (coerce (class-of a) (class-of b))
                           ((class-of a) (class-of b))
                           (lambda (fun r_ a_ b_) (binary-op fun r_ a_ b_ ADD))))
  (+ a b))
(define-method (+ (a <element>) (b <integer>))
  (+ a (make (match b) #:value b)))
(define-method (+ (a <integer>) (b <element>))
  (+ (make (match a) #:value a) b))

(define-method (- (a <element>))
  (add-method! - (jit-wrap ctx
                           (class-of a)
                           ((class-of a))
                           (lambda (fun r_ a_) (unary-op fun r_ a_ NEG))))
  (- a))
(define-method (- (a <element>) (b <element>))
  (add-method! - (jit-wrap ctx
                           (coerce (class-of a) (class-of b))
                           ((class-of a) (class-of b))
                           (lambda (fun r_ a_ b_) (binary-op fun r_ a_ b_ SUB))))
  (- a b))
(define-method (- (a <element>) (b <integer>))
  (- a (make (match b) #:value b)))
(define-method (- (a <integer>) (b <element>))
  (- (make (match a) #:value a) b))

(define (fill t n value); TODO: replace with tensor operation
  (let [(retval (make (sequence t) #:size n))]
    (store retval value)
    retval))
