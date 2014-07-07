# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

```Scheme
(use-modules (oop goops) (aiscm jit) (aiscm int))
(define ctx (make <jit-context>))
(define f (asm ctx <int> (list (MOV EAX EDI) (NEG EAX) (RET)) <int>))
(f 42)
; -42
```
