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

(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))

(define prog (list (ADD a b) (ADD a c) (RET)))
(define live (live-analysis prog))

(define colors (register-allocate prog #:registers (list RAX ESI)))
(define unassigned (find (compose not cdr) (reverse colors)))
(define participants ((adjacent (interference-graph live)) (car unassigned)))

(define spill-var (argmin (cut occurrences <> prog) participants))

(define prog (spill-variable spill-var 8 prog))

(define colors (register-allocate prog #:registers (list RAX ESI)))
(substitute-variables prog colors)

; #:predefined?


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
