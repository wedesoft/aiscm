#!/usr/bin/guile \
--no-auto-compile
!#
(use-modules (ice-9 format))
(use-modules (oop goops))
;(use-modules (aiscm element))
;(use-modules (aiscm int))
;(use-modules (aiscm jit))
;(use-modules (system foreign))
;(define c (make <jit-context>))

;(define-method (+ (a <int>) (b <int>))
;  (let ((f (asm c int (list (MOV EAX EDI) (ADD EAX ESI) (RET)) int int)))
;    (make <int> #:value (f (get-value a) (get-value b)))))

;(define i (make <int> #:value 42))
;(define j (make <int> #:value 13))

;(format #t "~a + ~a = ~a~&" (get-value i) (get-value j) (get-value (+ i j)))
;(format #t "~a + ~a = ~a~&" (get-value j) (get-value i) (get-value (+ j i)))
;(format #t "Compiled ~a method(s).~&" (length (slot-ref c 'binaries)))

(define (descriptor expr)
  (begin
    (display expr)
    (display "\n")
  (cond
    ((null? expr) "")
    ((pair? expr) (format #f "<~a_~a>"
                          (car expr)
                          (string-join (map descriptor (cdr expr)) "_")))
    (else "?")))
  )

(define (toplevel-define! name val)
  (module-define! (current-module) name val))

(define (dispatcher sym)
  (begin
    (if (not (defined? sym))
      (let ((gen (make <generic> #:name sym)))
        (toplevel-define! sym gen)
        (add-method! gen (method () 0))))
    sym))

;(define-syntax-rule (compile expr)
;  (let ((descr (string->symbol (descriptor (quote expr)))))
;    (dispatcher descr)
;    (eval (list descr) (current-module))))
;
;(define (f) (compile (+ i j)))
;(format #t "~s~&" (f))
;(format #t "~s~&" (f))
;(format #t "~s~&" (f))

(define-syntax compile
  (lambda (x)
    (syntax-case x ()
      ((_ expr)
       #`(begin
           #,(descriptor (syntax->datum #'expr)))))))

(define (f) (compile (+ i j)))
(format #t "~s~&" (f))
(format #t "~s~&" (f))
(format #t "~s~&" (f))
