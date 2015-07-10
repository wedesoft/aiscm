#!/usr/bin/guile \
--no-auto-compile
!#
(use-modules (ice-9 format))
(use-modules (oop goops))
;(use-modules (aiscm element))
;(use-modules (aiscm int))
;(use-modules (aiscm jit))
;(use-modules (system foreign))
;(define c (make <context>))

;(define-method (+ (a <int>) (b <int>))
;  (let ((f (asm c int (list (MOV EAX EDI) (ADD EAX ESI) (RET)) int int)))
;    (make <int> #:value (f (get-value a) (get-value b)))))

;(define i (make <int> #:value 42))
;(define j (make <int> #:value 13))

;(format #t "~a + ~a = ~a~&" (get-value i) (get-value j) (get-value (+ i j)))
;(format #t "~a + ~a = ~a~&" (get-value j) (get-value i) (get-value (+ j i)))
;(format #t "Compiled ~a method(s).~&" (length (slot-ref c 'binaries)))

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
