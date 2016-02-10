(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (ice-9 optargs) (ice-9 curried-definitions) (aiscm util) (aiscm element) (aiscm pointer) (aiscm mem) (aiscm sequence) (aiscm asm) (aiscm jit) (aiscm op) (aiscm int) (aiscm float) (aiscm rgb))

(use-modules (ice-9 optargs))
(define* (test #:key (x 1) (y 2) #:allow-other-keys #:rest r)
  (list x y r))

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

(define (expr->descr expr)
  (letrec
    ((expr->str
       (lambda (expr)
         (cond
           ((null? expr) "")
           ((pair? expr) (format #f "<~a_~a>"
                                 (car expr)
                                 (string-join (map expr->str (cdr expr)) "_")))
           (else "?")))))
    (format #t "~s~&" expr)
    (string->symbol (expr->str expr))))

(define (expr->params expr)
  (cond
    ((null? expr) '())
    ((pair? expr) (concatenate (map expr->params (cdr expr))))
    (else (list expr))))

(define-syntax compile
  (lambda (x)
    (syntax-case x ()
      ((k expr)
       #`(begin
           (define-method
             (#,(datum->syntax
                  #'k
                  (expr->descr (syntax->datum #'expr)))) 0)
           (#,(datum->syntax
                #'k
                (expr->descr (syntax->datum #'expr)))))))))

; * define-method (descr (a <element>) (b <element>))
;    (make (coerce (class-of a) (class-of b)) #:value (+ (get-value a) (get-value b))))

; (define-syntax t (lambda (x) (syntax-case x () ((k expr) (datum->syntax #'k (expr->call (syntax->datum #'expr)))))))

(define (ttt descr expr)
  (list 'define-method (cons descr (syntax->datum (generate-temporaries expr)))
        (make <int> #:value (+ (get-value a) (get-value b)))))

(define (f) (compile (+ i j)))
(format #t "~s~&" (f))
(format #t "~s~&" (f))
(format #t "~s~&" (f))
;(format #t "~s~&" <+_?_?>)

;(define-syntax compile (lambda (x) (syntax-case x () ((k expr) #`(syntax->datum #'expr)))))
