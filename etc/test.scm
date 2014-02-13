#!/usr/bin/guile \
--no-auto-compile
!#
(use-modules (ice-9 format))
(use-modules (oop goops))
(use-modules (aiscm element))
(use-modules (aiscm int))
(use-modules (aiscm jit))
(use-modules (system foreign))
(define c (make <jit-context>))
;(define-method (+ (a <int>) (b <int>))
;  (let ((f (asm c int (list (MOV EAX EDI) (ADD EAX ESI) (RET)) int int)))
;    (make <int> #:value (f (get-value a) (get-value b)))))
(define i (make <int> #:value 42))
(define j (make <int> #:value 13))
;(format #t "~a + ~a = ~a~&" (get-value i) (get-value j) (get-value (+ i j)))
;(format #t "~a + ~a = ~a~&" (get-value j) (get-value i) (get-value (+ j i)))
;(format #t "Compiled ~a method(s).~&" (length (slot-ref c 'binaries)))


(define-syntax-rule (compile exp) 0)
(define-syntax compile (lambda (x) 0))
(define-syntax compile (lambda (x) (syntax-case x () ((_ exp) 0))))
(define-syntax compile (lambda (x) (syntax-case x () ((_ exp) (syntax exp)))))

(define-syntax compile (lambda (x) (syntax-case x () ((_ exp) (syntax (car (quote exp)))))))
(define-syntax-rule (compile exp) (car (quote exp)))
(define-syntax-rule (compile exp) `(exp ,exp))

(compile (+ i j))
;(define-syntax when
;       (syntax-rules ()
;         ((when condition exp ...)
;          (if condition
;              (begin exp ...)))))
;
;     (when #t
;       (display "hey ho\n")
;       (display "let's go\n"))

;(define-syntax aif
;  (lambda (x)
;    (syntax-case x ()
;                 ((_ test then else)
;                  (with-syntax ((it (datum->syntax x 'it)))
;                               #'(let ((it test))
;                                   (if it then else)))))))

;(define-syntax compile
;  (syntax-rules ()
;    ((compile op a b)
;     (make <int> #:value (op (get-value a) (get-value b))))))

;(define-syntax compile
;  (lambda (x)
;    (syntax-case x ()
;      ((_ op a b)
;       (syntax (make <int> #:value (op (get-value a) (get-value b))))))))

;(define-syntax-rule (lazy exp)
;  (lambda () exp))

;(define-syntax descriptor
;  (lambda (x)
;    (syntax-case x ()
;      ((_ op a b)
;       (syntax op)))))

;(define-syntax compile
;  (lambda (x)
;    (syntax-case x ()
;      ((_ op a b)
;       (syntax (make <int> #:value (op (get-value a) (get-value b))))))))

;(format #t "~a~&" (compile + i j))
;(format #t "~a~&" (compile - i j))

