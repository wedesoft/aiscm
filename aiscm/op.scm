(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm jit)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (fill duplicate to-type bitwise-not is-zero? is-nonzero? do-not)
  #:re-export (+ - *))
(define ctx (make <jit-context>))

(define-method (dereference (self <var>)) self)
(define-method (dereference (self <pointer<>>)) (ptr (typecode self) (get-value self)))

(define-syntax-rule (element-wise (type p start n step) body ...)
  (env [(delta <long>)
        (stop  <long>)
        (incr  <long>)]
    (MOV delta n)
    (IMUL delta step)
    (LEA stop (ptr type start delta))
    (IMUL incr step (size-of type))
    (for [(p <long>) (MOV p start) (CMP p stop) (ADD p incr)] body ...)))

(define (movx a b)
  ((if (= (size-of (typecode a)) (size-of (typecode b)))
       MOV
       (if (signed? (typecode b)) MOVSX MOVZX))
   a b))

(define (copy-op r_ a_)
  (env [(r (typecode r_))]
    (movx r (dereference a_))
    (MOV (dereference r_) r)))
(define (destructive-unary-op op)
  (lambda (r_ a_)
    (env [(r (typecode r_))]
      (MOV r (dereference a_))
      (op r)
      (MOV (dereference r_) r))))
(define (copying-unary-op op)
  (lambda (r_ a_) (op (dereference r_) (dereference a_))))

(define-method (unary-op (r_ <pointer<>>) a_ op)
  (op r_ a_))
(define-method (unary-op (r_ <sequence<>>) (a_ <sequence<>>) op)
  (env [(*a  <long>)
        (a+  <long>)]
    (MOV *a (get-value a_))
    (MOV a+ (last (strides a_)))
    (IMUL a+ a+ (size-of (typecode a_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (unary-op (project (rebase *r r_)) (project (rebase *a a_)) op)
                  (ADD *a a+))))

(define (destructive-binary-op op)
  (lambda (r_ a_ b_)
    (env [(r (typecode r_))]
      (movx r (dereference a_))
      (if (= (size-of (typecode r_)) (size-of (typecode b_)))
        (op r (dereference b_))
        (env [(b (typecode r_))]
          (movx b (dereference b_))
          (op r b)))
      (MOV (dereference r_) r))))

(define-method (binary-op (r_ <pointer<>>) a_ b_ op)
  (op r_ a_ b_))
(define-method (binary-op (r_ <sequence<>>) (a_ <sequence<>>) (b_ <var>) op)
  (env [(*a  <long>)
        (a+  <long>)]
    (MOV *a (get-value a_))
    (MOV a+ (last (strides a_)))
    (IMUL a+ a+ (size-of (typecode a_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (binary-op (project (rebase *r r_)) (project (rebase *a a_)) b_ op)
                  (ADD *a a+))))
(define-method (binary-op (r_ <sequence<>>) (a_ <var>) (b_ <sequence<>>) op)
  (env [(*b  <long>)
        (b+  <long>)]
    (MOV *b (get-value b_))
    (MOV b+ (last (strides b_)))
    (IMUL b+ b+ (size-of (typecode b_)))
    (element-wise ((typecode r_) *r (get-value r_) (last (shape r_)) (last (strides r_)))
                  (binary-op (project (rebase *r r_)) a_ (project (rebase *b b_)) op)
                  (ADD *b b+))))
(define-method (binary-op (r_ <sequence<>>) (a_ <sequence<>>) (b_ <pointer<>>) op)
  (env [(b (typecode b_))]
    (MOV b (dereference b_))
    (binary-op r_ a_ b op)))
(define-method (binary-op (r_ <sequence<>>) (a_ <pointer<>>) (b_ <sequence<>>) op)
  (env [(a (typecode a_))]
    (MOV a (dereference a_))
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

(define-syntax-rule (define-unary-op name op conversion)
  (define-method (name (a <element>))
    (let* [(result-type (conversion (class-of a)))
           (fun         (wrap ctx <null> (list result-type (class-of a))
                              (lambda (r_ a_) (list (unary-op r_ a_ op) (RET)))))]
      (add-method! name
                   (make <method>
                         #:specializers (list (class-of a))
                         #:procedure (lambda (a)
                                       (let [(r (make result-type #:shape (shape a)))]
                                         (fun r a)
                                         r)))))
    (name a)))

(define (coerce-shapes a b)
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
                                         (let [(r (make result-type #:shape (coerce-shapes a b)))]
                                           (fun r (get a) (get b))
                                           r)))))
      (name a b))
    (define-method (name (a <element>) b) (name a (make (match b) #:value b)))
    (define-method (name a (b <element>)) (name (make (match a) #:value a) b))))

(define-unary-op duplicate copy-op identity)
(define-unary-op - (destructive-unary-op NEG) identity)
(define-unary-op bitwise-not (destructive-unary-op NOT) identity)
(define-unary-op is-zero? (copying-unary-op (lambda (r a) (list (CMP a 0) (SETE r)))) (cut to-type <> <bool>))
(define-unary-op is-nonzero? (copying-unary-op (lambda (r a) (list (CMP a 0) (SETNE r)))) (cut to-type <> <bool>))
(define do-not is-zero?)

; TODO: define-unary-op :not, :bool
; TODO: define-unary-op :conj
; TODO: define-unary-op :abs, :scalar
; TODO: define-unary-op :arg, :float-scalar
; TODO: define-unary-op :floor
; TODO: define-unary-op :ceil
; TODO: define-unary-op :round

(define-binary-op + (destructive-binary-op ADD))
(define-binary-op - (destructive-binary-op SUB))
(define-binary-op * (destructive-binary-op IMUL))

; TODO: define-binary-op :**, :coercion-maxint
; TODO: define-binary-op :/
; TODO: define-binary-op :%
; TODO: define-binary-op :fmod
; TODO: define-binary-op :and, :coercion-bool
; TODO: define-binary-op :or, :coercion-bool
; TODO: define-binary-op :&
; TODO: define-binary-op :|
; TODO: define-binary-op :^
; TODO: define-binary-op :<<
; TODO: define-binary-op :>>
; TODO: define-binary-op :eq, :coercion-bool
; TODO: define-binary-op :ne, :coercion-bool
; TODO: define-binary-op :<=, :coercion-bool
; TODO: define-binary-op :<, :coercion-bool
; TODO: define-binary-op :>=, :coercion-bool
; TODO: define-binary-op :>, :coercion-bool
; TODO: define-binary-op :minor
; TODO: define-binary-op :major

(define (fill type shape value); TODO: replace with tensor operation
  (let [(retval (make (multiarray type (length shape)) #:shape shape))]
    (store retval value)
    retval))

(define-method (to-type (self <meta<sequence<>>>) (target <meta<element>>))
  (multiarray target (dimension self)))
(define-method (to-type (self <sequence<>>) (target <meta<int<>>>))
  (let* [(result-type (to-type (class-of self) target))
         (proc
           (cond ((equal? (typecode self) target)
                  (lambda (self target) self))
                 ((= (size-of (typecode self)) (size-of target))
                  (lambda (self target)
                    (make result-type
                          #:shape (shape self)
                          #:strides (strides self)
                          #:value (get-value self))))
                 ((< (size-of (typecode self)) (size-of target))
                  (let [(fun (wrap ctx <null>
                                   (list result-type (class-of self))
                                   (lambda (r_ a_) (list (unary-op r_ a_ copy-op) (RET)))))]
                    (lambda (self target)
                      (let [(r (make result-type #:shape (shape self)))]
                        (fun r self)
                        r))))
                 (else
                  (lambda (self target)
                    (make result-type
                          #:shape (shape self)
                          #:strides (map (cut * (/ (size-of (typecode self))
                                                   (size-of target)) <>)
                                         (strides self))
                          #:value (get-value self))))))]
    (add-method! to-type
                 (make <method>
                       #:specializers (list (class-of self) (class-of target))
                       #:procedure proc))
    (to-type self target)))
