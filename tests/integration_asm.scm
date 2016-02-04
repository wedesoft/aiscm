(use-modules (oop goops) (aiscm asm) (aiscm int))
(define ctx (make <context>))
(define f (asm ctx <int> (list <int>) (list (MOV EAX EDI) (NEG EAX) (RET))))
(f 42)
; -42
