# Just-in-time Compiler

The following simple example shows how to declare a function returning a
constant value.

```Scheme
(use-modules (oop goops) (aiscm jit) (system foreign))
(define ctx (make <jit-context>))
(define f (asm ctx int (list (MOV EAX 42) (RET))))
(f)
; 42
```
