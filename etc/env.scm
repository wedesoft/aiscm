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

(- (list->multiarray '(((1 -2 3) (-4 5 -6)))))

(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))

(define prog (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET)))

(virtual-registers <null> '() (lambda () prog) #:registers (list RAX))

(map (idle-live prog (live-analysis prog)) (list a b c))

(define prog (list (MOV a 1) (MOV b 2) (ADD b 1) (ADD a 1) (RET)))

(define default-registers (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX RBP R12 R13 R14 R15))

(define* (xxx prog #:key (predefined '()) (registers default-registers))
  (let* [(live       (live-analysis prog))
         (conflicts  (interference-graph live))
         (colors     (color-graph conflicts registers #:predefined predefined))
         (unassigned (find (compose not cdr) (reverse colors)))]
    (if unassigned
      (let* [(participants ((adjacent (interference-graph live)) (car unassigned)))
             (spill-var    (argmax (idle-live prog live) participants))]
        (xxx (spill-variable spill-var 8 prog)
             #:predefined predefined
             #:registers registers))
      (save-and-use-registers prog colors))))

(define live (live-analysis prog))
(define colors (register-allocate prog #:registers (list RAX ESI)))
(define unassigned (find (compose not cdr) (reverse colors)))
(define participants ((adjacent (interference-graph live)) (car unassigned)))

(define spill-var (argmax (idle-live prog live) participants))

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
