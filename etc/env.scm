(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (aiscm util)
             (aiscm element)
             (aiscm pointer)
             (aiscm mem)
             (aiscm sequence)
             (aiscm jit)
             (aiscm int))
(define i 42)
(define j 42)
(define s (list->multiarray '(1 2 3)))
(define ctx (make <jit-context>))
(define sum
  (wrap ctx <ubyte> (list <ubyte> <ubyte>)
    (lambda (r a b)
      (list (MOV r a)
            (ADD r b)
            (RET)))))
(sum 2 3)

(define shp
  (wrap ctx <long> (list (sequence <ubyte>))
        (lambda (r s) (list (MOV r (car (shape s))) (RET)))))
(shp s)

; no return value
(define i1 (random (ash 1 30)))
(define i2 (random (ash 1 30)))
(define mem (make <mem> #:size 256))
(define iptr (make (pointer <int>) #:value mem))
(define (idata) (begin
                  (store iptr       i1)
                  (store (+ iptr 1) i2)
                  mem))
((asm ctx <null> (list <int>) (list (MOV RAX (idata)) (ADD (ptr <int> RAX) EDI) (RET))) i2)
(+ i1 i2)
(get-value (fetch iptr))

(wrap ctx <null> '() (lambda () (list (RET))))

;(define-syntax env
;  (lambda (x)
;    (syntax-case x (call)
;      ((_)                (syntax (list)))
;      ((_ (call x) y ...) (syntax (cons x (env y ...)))); detect upper case identifier
;      ((_ (x ...) y ...)  (syntax (cons (env x ...) (env y ...))))
;      ((_ x y ...)        (syntax (cons (quote x) (env y ...))))
;      )))
;(define-method (op (x <integer>)) (env (MOV AX (call x)) (RET))); replace x?
;; (define-method (op (x_ <integer>)) (env [(x (reg (get-value x_)))] (MOV AX x) (RET)))
;(d (op 5))
