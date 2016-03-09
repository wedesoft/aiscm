(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm jit)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm rgb)
  #:use-module (aiscm complex)
  #:use-module (aiscm sequence)
  #:export (fill duplicate ! ensure-default-strides)
  #:re-export (+ - * / % = < <= > >= min max))
(define ctx (make <context>))

(define-method (to-type (target <meta<element>>) (self <sequence<>>))
  (let [(proc (let [(fun (jit ctx (list (class-of self)) (cut to-type target <>)))]
                (lambda (target self) (fun self))))]
    (add-method! to-type
                 (make <method>
                       #:specializers (list (class-of target) (class-of self))
                       #:procedure proc))
    (to-type target self)))

(define (fill type shape value)
  (let [(retval (make (multiarray type (length shape)) #:shape shape))]
    (store retval value)
    retval))
(define-syntax-rule (define-unary-op name op)
  (define-method (name (a <element>))
    (let [(f (jit ctx (list (class-of a)) op))]
      (add-method! name
                   (make <method>
                         #:specializers (list (class-of a))
                        #:procedure f)))
    (name a)))
;(define-unary-op duplicate identity)
;(define-unary-op - -)
;(define-unary-op ~ ~)
;(define-unary-op =0 =0)
;(define-unary-op !=0 !=0)
;(define-unary-op conj conj)
;(define ! =0)
;(define-syntax-rule (capture-binary-argument name type)
;  (begin
;    (define-method (name (a <element>) (b type)) (name a (make (match b) #:value b)))
;    (define-method (name (a type) (b <element>)) (name (make (match a) #:value a) b))))
;(define-syntax-rule (define-binary-op name op)
;  (begin
;    (define-method (name (a <element>) (b <element>))
;      (let [(f (jit ctx (map class-of (list a b)) op))]
;        (add-method! name
;                     (make <method>
;                           #:specializers (map class-of (list a b))
;                           #:procedure (lambda (a b) (f (get a) (get b))))))
;      (name a b))
;    (capture-binary-argument name <boolean>)
;    (capture-binary-argument name <integer>)
;    (capture-binary-argument name <real>)
;    (capture-binary-argument name <rgb>)
;    (capture-binary-argument name <complex>)))

(define-method (+ (a <element>) (b <integer>)) (+ a (make (match b) #:value b)))
(define-method (+ (a <integer>) (b <element>)) (+ (make (match a) #:value a) b))
(define-method (+ (a <sequence<>>) (b <element>))
  (let [(f (jit ctx (map class-of (list a b)) +))]
    (add-method! +
                 (make <method>
                       #:specializers (map class-of (list a b))
                       #:procedure (lambda (a b) (f (get a) (get b)))))
    (+ a b)))
(define-method (+ (a <element>) (b <sequence<>>))
  (let [(f (jit ctx (map class-of (list a b)) +))]
    (add-method! +
                 (make <method>
                       #:specializers (map class-of (list a b))
                       #:procedure (lambda (a b) (f (get a) (get b)))))
    (+ a b)))
;(define-binary-op +   +)
;(define-binary-op -   -)
;(define-binary-op *   *)
;(define-binary-op &   &)
;(define-binary-op |   |)
;(define-binary-op ^   ^)
;(define-binary-op <<  <<)
;(define-binary-op >>  >>)
;(define-binary-op /   /)
;(define-binary-op %   %)
;(define-binary-op =   =)
;(define-binary-op !=  !=)
;(define-binary-op <   <)
;(define-binary-op <=  <=)
;(define-binary-op >   >)
;(define-binary-op >=  >=)
;(define-binary-op &&  &&)
;(define-binary-op ||  ||)
;(define-binary-op max max)
;(define-binary-op min min)

(define (ensure-default-strides img)
  (if (equal? (strides img) (default-strides (shape img))) img (duplicate img)))

(define (slice arr i n)
  (make (to-type (base (typecode arr)) (class-of arr))
        #:shape (shape arr)
        #:strides (map (cut * n <>) (strides arr))
        #:value (+ (value arr) (* i (size-of (base (typecode arr)))))))
(define-syntax-rule (slice-if-type type arr i n default) (if (is-a? (typecode arr) (class-of type)) (slice arr i n) default))
(define-method (red   (self <sequence<>>)) (slice-if-type <rgb<>> self 0 3 self))
(define-method (green (self <sequence<>>)) (slice-if-type <rgb<>> self 1 3 self))
(define-method (blue  (self <sequence<>>)) (slice-if-type <rgb<>> self 2 3 self))
(define-method (real-part (self <sequence<>>)) (slice-if-type <complex<>> self 0 2 self))
(define-method (imag-part (self <sequence<>>)) (slice-if-type <complex<>> self 1 2 (fill (typecode self) (shape self) 0)))
