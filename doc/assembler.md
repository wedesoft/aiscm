# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

```Scheme
@../tests/integration_asm.scm@
```

This will generate and call a function with the following assembler code:

```Assembler
mov eax,edi
neg eax
ret
```
