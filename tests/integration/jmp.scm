(use-modules (oop goops) (aiscm asm) (aiscm int))
(define ctx (make <context>))
(define f (asm ctx <int> (list <int>) (list (MOV EAX EDI) (CMP EAX 0) (JNLE 'skip) (NEG EAX) 'skip (RET))))
(f -42)
; 42
(f 42)
; 42

