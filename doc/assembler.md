# Built-in assembler

The built-in Intel x86-64 assembler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using the built-in assembler.
The function then is used to negate the number *42*.

Setting the environment variable *DEBUG* to *YES* will cause the JIT compiler to display the LLVM intermediate code.

## Virtual registers

The *jit* function provides an interface to LLVM.
The following example defines a function for adding two integers:

```Scheme
@../tests/integration/virtual_registers.scm@
```

The *jit* function also instantiates loops for array processing. For example:

```Scheme
@../tests/integration/virtual_arrays.scm@
```

## Interpreter callbacks

Callbacks into the *GNU Guile* interpreter are defined as well. They can be used to generate machine code which handles Scheme objects:

```
@../tests/integration/callback.scm@
```
