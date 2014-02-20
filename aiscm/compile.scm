(define-module (aiscm compile)
  #:use-module (oop goops)
  #:export (expr->params
            expr->descr))
(define (expr->params expr)
  (cond
    ((null? expr) '())
    ((pair? expr) (apply append (map expr->params (cdr expr))))
    (else (list expr))))
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
    (string->symbol (expr->str expr))))

