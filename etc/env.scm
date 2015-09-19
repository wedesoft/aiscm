(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (ice-9 optargs) (ice-9 curried-definitions) (aiscm util) (aiscm element) (aiscm pointer) (aiscm mem) (aiscm sequence) (aiscm asm) (aiscm jit) (aiscm op) (aiscm int) (aiscm float) (aiscm rgb))

(define ctx (make <context>))

(define target <intrgb>)
(define frag (to-type target (parameter (skel (sequence <bytergb>)))))

(define classes (list (sequence <bytergb>)))
(define vars (map skel classes))
(define frag (apply (cut to-type <intrgb> <>) (map parameter vars)))
(define return-type (type frag))
(define retval (skel return-type))

(store (skel (sequence <intrgb>)) (to-type target frag))

((jit ctx (list <bytergb>) red) (rgb 1 2 3))

(use-modules (rnrs bytevectors) (ice-9 binary-ports))
(define (dump code) (let [(filename (tmpnam))]
  (call-with-output-file filename (cut put-bytevector <> (u8-list->bytevector (flatten code))))
  (system (format #f "objdump -D -b binary -Mintel -mi386:x86-64 ~a" filename))))

; macros: (jit-method [(x <int>) (y <float>)] (+ x y))

;(define-syntax let-vars
;  (syntax-rules ()
;    ((let-vars [(name type) vars ...] body ...)
;     (let [(name (make <var> #:type type #:symbol (quote name)))]
;       (let-vars [vars ...] body ...)))
;    ((let-vars [] body ...)
;     (begin body ...))))


(define-syntax test
  (syntax-rules ()
    ((test ((a b) args ...))
     (cons b (test (args ...))))
    ((test  ())
     '())))

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
