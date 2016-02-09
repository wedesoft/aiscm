# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

## Function definition

```Scheme
@../tests/integration_asm.scm@
```

This will generate and call a function with the following assembler code.
Here is an example function negating an integer:

```Assembler
mov eax,edi
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
