(define-module (aiscm compile)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:export (expr->params
            expr->descr
            expr->temps
            expr->call
            expr->method)
  #:export-syntax (comp))
(define (expr->params expr)
  (cond
    ((null? expr) '())
    ((pair? expr) (apply append (map expr->params (cdr expr))))
    (else (list expr))))
(define (expr->descr expr)
  (letrec
    [(expr->str
       (lambda (expr)
         (cond
           ((null? expr) "")
           ((pair? expr) (format #f "<~a_~a>"
                                 (car expr)
                                 (string-join (map expr->str (cdr expr)) "_")))
           (else "?"))))]
    (string->symbol (expr->str expr))))
(define (expr->temps expr)
  (syntax->datum (generate-temporaries (expr->params expr))))
(define (expr->call expr)
  (cons (expr->descr expr) (expr->params expr)))
(define (expr->method expr)
  (let [(descr (expr->descr expr))
        (temps (expr->temps expr))
        (param (lambda (arg) `(,arg <element>)))
        (extract (lambda (arg) `(get-value ,arg)))]
    `(define-method (,descr ,@(map param temps))
                    (make <int> #:value (+ ,@(map extract temps))))))
(define-syntax comp
  (lambda (x)
    (syntax-case x ()
      ((k expr)
       #`(begin
           #,(datum->syntax #'k (expr->method (syntax->datum #'expr)))
           #,(datum->syntax #'k (expr->call (syntax->datum #'expr))))))))
