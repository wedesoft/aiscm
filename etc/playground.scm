(use-modules (oop goops)
             (srfi srfi-1)
             (aiscm sequence)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm int)
             (aiscm jit)
             (aiscm util))

(test-begin "playground")

(define ctx (make <context>))

(define a (parameter <int>))
(define b (parameter <int>))
(define r (parameter <int>))

(asm ctx <int> (list <int> <int>) (list (MOV EAX EDI) (ADD EAX ESI) (RET)))


(test-end "playground")
