#!/usr/bin/guile \
--no-auto-compile
!#
(use-modules (ice-9 format))
;(use-modules (oop goops))
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
  (cond
    ((null? expr) "")
    ((pair? expr) (format #f "<~a_~a>"
                          (car expr)
                          (string-join (map descriptor (cdr expr)) "_")))
    (else "?")))

(define-syntax-rule (compile expr)
  (descriptor (quote expr)))

(format #t "~s~&" (compile ()))
(format #t "~s~&" (compile i))
(format #t "~s~&" (compile (+ i j)))
(format #t "~s~&" (compile (+ i (* j k))))

