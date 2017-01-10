(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (system foreign)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm mem)
             (aiscm pointer)
             (aiscm rgb)
             (aiscm obj)
             (aiscm asm)
             (aiscm jit)
             (aiscm method)
             (aiscm util)
             (guile-tap))


; remove predefinitions where register is blocked
; add code for moving values

(define (blocked-predefined predefined intervals blocked)
  "Get blocked predefined registers"
  (filter (compose not null? (overlap-interval blocked) (cut assq-ref intervals <>) car) predefined))

(define (move-blocked-predefined blocked-predefined)
  "Generate code for blocked predefined variables"
  (map (compose MOV car+cdr) blocked-predefined))

(define (non-blocked-predefined predefined blocked-predefined)
  "Compute the set difference of the predefined variables and the variables with blocked registers"
  (lset-difference equal? predefined blocked-predefined))

(ok (null? (blocked-predefined '() '() '()))
    "no predefined variables")
(ok (equal? (list (cons 'a RDI))
            (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(1 . 3)))))
    "detect predefined variable with blocked register")
(ok (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) '()))
    "ignore predefined variables if no registers are blocked")
(ok (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(3 . 4)))))
    "ignore predefined variables if the associated register is not blocked")

(define a (var <int>))

(ok (null? (move-blocked-predefined '()))
    "no predefined variables with blocked registers to move")
(ok (equal? (list (MOV a RAX)) (move-blocked-predefined (list (cons a RAX))))
    "copy variable from blocked register")

(ok (equal? (list (cons 'a RDI)) (non-blocked-predefined (list (cons 'a RDI)) '()))
    "no predefinitions to discard")
(ok (equal? '() (non-blocked-predefined (list (cons 'a RDI)) (list (cons 'a RDI))))
    "discard predefined variables which are blocked")
(ok (equal? (list (cons 'b RSI)) (non-blocked-predefined (list (cons 'a RDI) (cons 'b RSI)) (list (cons 'a RDI))))
    "only discard predefined variables which are blocked")

(run-tests)
