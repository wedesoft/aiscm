# LLVM JIT compiler

The LLVM JIT compiler facilitates performant implementations of numerical methods.
The following example shows a function for negating integers implemented using LLVM.
The function then is used to negate the number *42*.

Setting the environment variable *DEBUG* to *YES* will cause the JIT compiler to display the LLVM intermediate code.

## Compilation

The *jit* function provides an interface to LLVM.
The following example defines a function for adding two integers:

```Scheme
@../tests/integration/virtual_registers.scm@
```

The *jit* function also instantiates loops for array processing. For example:

```Scheme
@../tests/integration/virtual_arrays.scm@
```

## Scheme callbacks

Operations on Scheme objects are facilitated by callbacks into the Scheme interpreter:

```Scheme
@../tests/integration/callback.scm@
```

Callbacks into Scheme are used when handling arrays of Scheme objects:

```Scheme
@../tests/integration/object_array.scm@
```
