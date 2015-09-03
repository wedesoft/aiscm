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
  #:use-module (aiscm sequence)
  #:export (fill duplicate !)
  #:re-export (+ - * / = < <= > >=))
(define ctx (make <context>))

(define-method (to-type (target <meta<int<>>>) (self <sequence<>>))
  (let [(proc
          (if (>= (size-of (typecode self)) (size-of target))
              (let [(ratio (/ (size-of (typecode self)) (size-of target)))]
                (lambda (target self)
                  (make (to-type target (class-of self))
                        #:shape (shape self)
                        #:strides (map (cut * ratio <>) (strides self))
                        #:value (get-value self))))
              (let [(fun (jit ctx (list (class-of self)) (cut to-type target <>)))]
                (lambda (target self) (fun self)))))]
    (add-method! to-type
                 (make <method>
                       #:specializers (list (class-of target) (class-of self))
                       #:procedure proc))
    (to-type target self)))

(define (fill type shape value); TODO: replace with tensor operation
  (let [(retval (make (multiarray type (length shape)) #:shape shape))]
    (store retval value)
    retval))
(define-syntax-rule (def-unary-op name op)
  (define-method (name  (a <element>))
    (let [(f (jit ctx (list (class-of a)) op))]
      (add-method! name
                   (make <method>
                         #:specializers (list (class-of a))
                        #:procedure f)))
    (name a)))
(def-unary-op duplicate identity)
(def-unary-op - -)
(def-unary-op ~ ~)
(def-unary-op =0 =0)
(def-unary-op !=0 !=0)
(define ! =0)
(define-syntax-rule (def-binary-op name op)
  (begin
    (define-method (name (a <element>) (b <element>))
      (let [(f (jit ctx (list (class-of a) (class-of b)) op))]
        (add-method! name
                     (make <method>
                           #:specializers (list (class-of a) (class-of b))
                           #:procedure (lambda (a b) (f (get a) (get b))))))
      (name a b))
    (define-method (name (a <element>) b) (name a (make (match b) #:value b)))
    (define-method (name a (b <element>)) (name (make (match a) #:value a) b))))
(def-binary-op + +)
(def-binary-op - -)
(def-binary-op * *)
(def-binary-op & &)
(def-binary-op | |)
(def-binary-op ^ ^)
(def-binary-op / /)
(def-binary-op = =)
(def-binary-op != !=)
(def-binary-op < <)
(def-binary-op <= <=)
(def-binary-op > >)
(def-binary-op >= >=)
(def-binary-op && &&)
(def-binary-op || ||)
