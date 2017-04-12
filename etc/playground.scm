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

(test-begin "playground")

;(define-method (tensor-op name . args)
;  (if (not (defined? name))
;    (let [(f (jit ctx (map class-of args) (car args)))]
;      (add-method! name (make <method> #:specializers (map class-of args)
;                                       #:procedure (lambda args (apply f args))))))
;  (apply (eval name (current-module)) args))
;
;(define-macro (tensor expr) `(tensor-op '- ,expr))
;
;(define s (seq 2 3 5))
;
;(tensor s)
(define (tensor-operation? sym)
  (memv sym '(+ -)))

(define (expression->identifier expr)
  (cond
    ((list?             expr) (string-append "(" (string-join (map expression->identifier expr)) ")"))
    ((tensor-operation? expr) (symbol->string expr))
    (else                     "_")))

(test-assert "+ is a tensor operation"
  (tensor-operation? '+))
(test-assert "+ is a tensor operation"
  (tensor-operation? '-))
(test-assert "x is not a tensor operation"
  (not (tensor-operation? 'x)))

(test-equal "filter variable names in expression"
  "_" (expression->identifier 'x))
(test-equal "filter numeric arguments of expression"
  "_" (expression->identifier 0))
(test-equal "preserve unary plus operation"
  "(+ _)" (expression->identifier '(+ x)))
(test-equal "preserve unary minus operation"
  "(- _)" (expression->identifier '(- x)))
(test-equal "preserve binary plus operation"
  "(+ _ _)" (expression->identifier '(+ x y)))
(test-equal "works recursively"
  "(+ (- _) _)" (expression->identifier '(+ (- x) y)))

(test-end "playground")
