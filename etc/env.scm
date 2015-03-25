(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 optargs)
             (aiscm util)
             (aiscm element)
             (aiscm pointer)
             (aiscm mem)
             (aiscm sequence)
             (aiscm jit)
             (aiscm op)
             (aiscm int))
(define s (list->multiarray '(-1 2 3)))
(define m (list->multiarray '((-1 2) (3 4))))
(define ctx (make <jit-context>))

(define names '(a b c d e f g h i))
(define vars (map (cut make <var> #:type <int> #:symbol <>) names))

(drop vars 6)

(MOV a (ptr <int> RSP #x8))

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
