(use-modules (oop goops)
             (aiscm util)
             (aiscm jit)
             (aiscm int))
(define i (make <int> #:value 42))
(define ctx (make <jit-context>))

(define (wrap ctx return-type argument-types proc)
  (let* [(r    (make <var> #:type return-type))
         (args (map (lambda (type) (make <var> #:type type)) argument-types))
         (prog (apply proc (cons r args)))
         (code (subst prog (list (cons r RAX))))]
  (asm ctx return-type argument-types code)))

(define f (wrap ctx <int> (list <int>) (lambda (r x) (list (MOV r x) (RET)))))

;(wrap ctx
;      <int>
;      (list <int>)
;      (lambda (f r x)
;        (let [(v (make <var> #:type <int> #:symbol 'v))])
;        (MOV v x)
;        (ADD v 42)
;        (MOV r v)))

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
