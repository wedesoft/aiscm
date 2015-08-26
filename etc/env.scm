(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (ice-9 optargs) (ice-9 curried-definitions) (aiscm util) (aiscm element) (aiscm pointer) (aiscm mem) (aiscm sequence) (aiscm asm) (aiscm jit) (aiscm op) (aiscm int) (aiscm float))

(define ctx (make <context>))

(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))
(define x (make <var> #:type <long> #:symbol 'x))
(define y (make <var> #:type <long> #:symbol 'y))
(define p (make <var> #:type <long> #:symbol 'p))
(define q (make <var> #:type <long> #:symbol 'q))
(define *p (make (pointer <int>) #:value p))
(define *q (make (pointer <int>) #:value q))
(define s (param (sequence <int>) (list x y p)))
(define r (param (sequence <int>) (list x y q)))

(store r (typecast <int> (parameter s)))

((jit ctx (list (sequence <int>)) identity) (seq <int> 1 2 3))

(assemble r (list s) (parameter s))

((jit ctx (list (sequence <int>)) identity) (seq <int> 1 2 3))

; code: store object -> machine code
; #:code (lambda (store) (store (lambda (result) ... )))
; store: make result var, call code
; store: loop (initialise, rebase, project), make result var, call code, write to memory
; (accessors s) -> ((pointer stride count) ...) which pointer?
; (tensor [i] ((roll m) i))
; (tensor [i] (get m i 1))
; (tensor [i j] (* (s i) (s j)))
; (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))

;(env [(*p <long>)]
;  (store (project (rebase *p s)) (project ...)))

(project s)

(define (coerce-shapes a b)
  (let [(shape-a (shape a))
        (shape-b (shape b))]
    (if (>= (length shape-a) (length shape-b)) shape-a shape-b)))

(define-syntax-rule (element-wise (type p start n step) body ...)
  (env [(delta <long>)
        (stop  <long>)
        (incr  <long>)]
    (MOV delta n)
    (IMUL delta step)
    (LEA stop (ptr type start delta))
    (IMUL incr step (size-of type))
    (for [(p <long>) (MOV p start) (CMP p stop) (ADD p incr)] body ...)))

(define-syntax tensor-list
  (syntax-rules ()
    ((_ arg args ...) (cons (tensor arg) (tensor-list args ...)))
    ((_) '())))

(define-syntax tensor
  (syntax-rules (*)
    ((_ (* args ...)) (cons '* (tensor-list args ...)))
    ((_ arg)          arg)))

(define s (seq 1 2 3 4))
(define-syntax-rule (tensor sym args ...)
  (if (is-a? sym <element>) (get sym args ...) (sym args ...)))
(tensor + s 1)
(tensor s 1)
(tensor (seq 1 2 3 4) 1)
(tensor + (s 1) 1)

; arrays
(make-array 0 2 3)
(make-typed-array 'u8 0 2 3)
#vu8(1 2 3)
#2((1 2 3) (4 5 6))

#2u32((1 2 3) (4 5 6))
(define m #2s8((1 -2 3) (4 5 6)))
(array-ref m 1 0)
(array-shape m)
(array-dimensions m)
(array-rank m)
(array->list m)

; monkey patching
(class-slots <x>)
(define m (car (generic-function-methods test)))
((method-procedure m) x)
(slot-ref test 'methods)
;(sort-applicable-methods test (compute-applicable-methods test (list x)) (list x))
(equal? (map class-of (list x)) (method-specializers m))
(define x (make <x>))
(test x)
(define-method (test (x <x>)) 'test2)
(test x)
