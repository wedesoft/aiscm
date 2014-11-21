# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

```Scheme
(use-modules (oop goops) (aiscm jit) (aiscm int))
(define ctx (make <jit-context>))
(define f (asm ctx <int> (list <int>) (list (MOV EAX EDI) (NEG EAX))))
(f 42)
; -42
```

This will generate and call a function with the following assembler code:

```Assembler
mov eax,edi
neg eax
ret
```
