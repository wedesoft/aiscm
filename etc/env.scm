(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm sequence) (aiscm pointer) (aiscm element) (aiscm int) (aiscm op) (aiscm util) (srfi srfi-1))

(define-method (parameter self)
  (make (fragment (class-of self)) #:args (list self) #:name parameter #:code '() #:value (get self)))
(define-class <elementwise> ()
  (setup     #:init-keyword #:setup     #:getter get-setup)
  (increment #:init-keyword #:increment #:getter get-increment)
  (body      #:init-keyword #:body      #:getter get-body))
(define-method (element-wise self)
  (make <elementwise> #:setup '() #:increment '() #:body self))
(define-method (element-wise (s <sequence<>>))
  (let [(incr (var <long>))
        (p    (var <long>))]
    (make <elementwise>
          #:setup (list (IMUL incr (last (strides s)) (size-of (typecode s)))
                        (MOV p (slot-ref s 'value)))
          #:increment (list (ADD p incr))
          #:body (project (rebase p s)))))
(define-method (element-wise (self <fragment<sequence<>>>))
  (let [(loops (map element-wise (get-args self)))]
    (make <elementwise>
          #:setup (map get-setup loops)
          #:increment (map get-increment loops)
          #:body (apply (get-name self) (map get-body loops)))))
(define-method (store (s <sequence<>>) (a <fragment<sequence<>>>))
  (let [(destination (element-wise s))
        (source      (element-wise a))]
    (list (get-setup destination)
          (get-setup source)
          (repeat (last (shape s))
                  (append (store (get-body destination) (get-body source))
                          (get-increment destination)
                          (get-increment source))))))

(define-class <lookup> ()
  (body #:init-keyword #:body #:getter get-body)
  (var #:init-keyword #:var #:getter get-var))
(define-method (get (self <fragment<sequence<>>>) (i <var>))
  (make <lookup> #:body self #:var i))

(define ctx (make <context>))
((jit ctx (list (sequence <int>)) identity) (seq <int> 2 3 5))

(define r (skeleton (sequence <int>)))
(define s (parameter (skeleton (sequence <int>))))
(define c (parameter (skeleton <int>)))

(define i (var <int>))


(get s i)

; (get s i)
; (tensor i ...)


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
