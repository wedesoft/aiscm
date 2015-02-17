(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (aiscm util)
             (aiscm element)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm jit)
             (aiscm int))
(define i 42)
(define s (list->multiarray '(1 2 3)))

; (match 42) -> <int>
; (match s) -> (sequence <byte>)
; (match '(1 2 3)) -> ???

(define c (map match (list i s)))
(define t (concatenate (map types c)))
(define v (map (cut make <var> #:type <>) t))



;(define (plus r a x)
;
;)

(define a (collate c v))
(car a)
(get-value (cadr a))
(shape (cadr a))
(strides (cadr a))


(content s)

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
