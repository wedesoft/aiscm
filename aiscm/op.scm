(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
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
  #:export (fill duplicate to-type ~ =0 !=0 ! != & | ^ && ||)
  #:re-export (+ - * / = < <= > >=))
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

(define ((cmp-setcc setcc-signed setcc-unsigned) r a b)
  (list (CMP a b) ((if (or (signed? (typecode a)) (signed? (typecode b))) setcc-signed setcc-unsigned) r)))
(define ((test-setcc setcc) r a) (list (TEST a a) (setcc r)))
(define ((test-booleans comb) r a b)
  (env [(r1 (typecode r))
        (r2 (typecode r))]
    (list (TEST a a) (SETNE r1) (TEST b b) (SETNE r2) (comb r1 r2) (MOV r r1))))

(define (divide r a b)
  (let* [(size (size-of (typecode r)))
         (ax   (reg size 0))
         (dx   (reg size 2))]
    (blocked RAX
      (if (signed? (typecode r))
        (if (= size 1)
          (list
            (MOV AL a)
            (CBW)
            (IDIV b)
            (MOV r AL))
          (list
            (MOV ax a)
            (blocked RDX
              (case size ((2) (CWD)) ((4) (CDQ)) ((8) (CQO)))
              (IDIV b)
              (MOV r ax))))
        (if (= size 1)
          (list
            (MOVZX AX a)
            (DIV b)
            (MOV r AL))
          (list
            (MOV ax a)
            (blocked RDX
              (MOV dx 0)
              (DIV b)
              (MOV r ax))))))))

(define (copy-op r_ a_)
  (env [(r (typecode r_))]
    (movx r (dereference a_))
    (MOV (dereference r_) r)))
(define ((destructive-unary-op op) r_ a_)
  (env [(r (typecode r_))]
    (MOV r (dereference a_))
    (op r)
    (MOV (dereference r_) r)))
(define ((copying-unary-op op) r_ a_)
  (env [(a (typecode a_))]
    (MOV a (dereference a_))
    (op (dereference r_) a)))

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

(define ((destructive-binary-op op) r_ a_ b_)
  (env [(r (typecode r_))
        (b (typecode r_))]
    (movx r (dereference a_))
    (movx b (dereference b_))
    (op r b)
    (MOV (dereference r_) r)))
(define ((copying-binary-op intermediate op) r_ a_ b_)
  (env [(a (intermediate (typecode a_) (typecode b_)))
        (b (intermediate (typecode a_) (typecode b_)))]
    (movx a (dereference a_))
    (movx b (dereference b_))
    (op (dereference r_) a b)))

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

(define-syntax-rule (define-binary-op name op coercion)
  (begin
    (define-method (name (a <element>) (b <element>))
      (let* [(result-type (coercion (class-of a) (class-of b)))
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

(define to-bool (cut to-type <> <bool>))
(define (sign-space a b)
  (let [(t (coerce a b))]
  (if (eq? (signed? (typecode a)) (signed? (typecode b)))
    t
    (integer (max 32 (bits t)) (if (signed? t) signed unsigned)))))

(define-unary-op duplicate copy-op identity)
(define-unary-op - (destructive-unary-op NEG) identity)
(define-unary-op ~ (destructive-unary-op NOT) identity)
(define-unary-op =0 (copying-unary-op (test-setcc SETE)) to-bool)
(define-unary-op !=0 (copying-unary-op (test-setcc SETNE)) to-bool)
(define ! =0)

; TODO: unary operation conj
; TODO: unary operation abs (scalar)
; TODO: unary operation arg (float-scalar)
; TODO: unary operation floor
; TODO: unary operation ceil
; TODO: unary operation round

(define-binary-op + (destructive-binary-op ADD) coerce)
(define-binary-op - (destructive-binary-op SUB) coerce)
(define-binary-op * (destructive-binary-op IMUL) coerce)
(define-binary-op & (destructive-binary-op AND) coerce)
(define-binary-op | (destructive-binary-op OR) coerce)
(define-binary-op ^ (destructive-binary-op XOR) coerce)
(define-binary-op / (copying-binary-op coerce divide) coerce)
(define-binary-op = (copying-binary-op coerce (cmp-setcc SETE SETE)) (compose to-bool coerce))
(define-binary-op != (copying-binary-op coerce (cmp-setcc SETNE SETNE)) (compose to-bool coerce))
(define-binary-op < (copying-binary-op sign-space (cmp-setcc SETL SETB)) (compose to-bool coerce))
(define-binary-op <= (copying-binary-op sign-space (cmp-setcc SETLE SETBE)) (compose to-bool coerce)); TODO: int32+ (with test)
(define-binary-op > (copying-binary-op sign-space (cmp-setcc SETNLE SETNBE)) (compose to-bool coerce))
(define-binary-op >= (copying-binary-op sign-space (cmp-setcc SETNL SETNB)) (compose to-bool coerce))
(define-binary-op && (copying-binary-op coerce (test-booleans AND)) (compose to-bool coerce))
(define-binary-op || (copying-binary-op coerce (test-booleans OR)) (compose to-bool coerce))

; TODO: binary operation ** (coercion-maxint)
; TODO: binary operation %
; TODO: binary operation fmod
; TODO: binary operation and (coercion-bool)
; TODO: binary operation or (coercion-bool)
; TODO: binary operation <<
; TODO: binary operation >>
; TODO: conditional -> minor, major
