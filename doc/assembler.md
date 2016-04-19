# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

## Function definition

Here is an example function negating an integer:

```Scheme
@../tests/integration_asm.scm@
```

This will generate and call an identity function with the following assembler code

```Assembler
mov eax,edi
neg eax
ret
```

It is also possible to use jump statements and labels:

```Scheme
@../tests/integration_jmp.scm@
```

This will generate and call a function for computing the absolute value. The assembler code is as follows

```Assembler
mov eax, edi
cmp eax, 0x0
jg 0xb
neg eax
ret
```

## Virtual registers

The *jit* function provides a higher level interface with virtual registers and operator mappings.
The following example defines a function for adding two integers:

```Scheme
@../tests/integration_virtual_registers.scm@
```

The *jit* function also instantiates loops for array processing. For example:

```Scheme
@../tests/integration_virtual_arrays.scm@
```
