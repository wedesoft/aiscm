(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (aiscm util)
             (aiscm element)
             (aiscm pointer)
             (aiscm mem)
             (aiscm sequence)
             (aiscm jit)
             (aiscm op)
             (aiscm int))
(define s (list->multiarray '(-1 2 3)))
(define ctx (make <jit-context>))

(define-method (unary-op (r_ <pointer<>>) (a_ <pointer<>>))
  (let [(r (make <var> #:type <byte> #:symbol 'r ))]
     (list (MOV r (ptr <byte> (get-value a_)))
           (NEG r)
           (MOV (ptr <byte> (get-value r_)) r))))

(define-method (unary-op (r_ <sequence<>>) (a_ <sequence<>>))
  (let [(*r (make <var> #:type <long> #:symbol '*r))
        (*a  (make <var> #:type <long> #:symbol '*a))
        (*rx (make <var> #:type <long> #:symbol '*rx))]
    (list (MOV *r (get-value r_))
          (MOV *a (get-value a_))
          (LEA *rx (ptr <byte> *r (last (shape r_))))
          'loop
          (CMP *r *rx)
          (JE 'return)
          (unary-op (project (rebase *r r_)) (project (rebase *a a_)))
          (ADD *r 1)
          (ADD *a 1)
          (JMP 'loop)
          'return)))

(define-method (- (s <element>))
  (let [(r (make (sequence <byte>) #:size (last (shape s))))]
    ((wrap ctx <null> (list (sequence <byte>) (sequence <byte>))
      (lambda (r_ a_)
        (let [(*r  (make <var> #:type <long> #:symbol '*r))
              (*a  (make <var> #:type <long> #:symbol '*a))
              (*rx (make <var> #:type <long> #:symbol '*rx))]
          (list (unary-op r_ a_)
                (RET))))) r s)
    r))
(neg s)


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
