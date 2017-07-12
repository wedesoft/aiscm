# Array operations
## Unary operations

The unary operations are:

* **-**: negation
* **~**: bitwise not
* **=0**: (or **!**): check equal to zero
* **!=0**: test not zero
* **red**: extract red channel of RGB value
* **green**: extract green channel of RGB value
* **blue**: extract blue channel of RGB value
* **real-part**: get real part of complex number
* **imag-part**: get imaginary part of complex number
* **conj**: complex conjugate

Here are a few examples using those operations:

```Scheme
@../tests/integration/unary_ops.scm@
```

Applied to the following image ...

![star-ferry.jpg](star-ferry.jpg "Test input image")

... inverting the RGB values yields the following image:

![inverted.jpg](inverted.jpg "Inverted image")

```Scheme
@../tests/integration/invert.scm@
```

## Binary operations

The binary operations are:

* **+:** addition
* **-:** subtraction
* **\*:** multiplication
* **/:** division
* **%:** remainder
* **<<:** shift left
* **>>:** shift right
* **&:** bitwise and
* **|:** bitwise or
* **^:** bitwise exclusive-or
* **&&:** boolean and
* **||:** boolean or
* **==:** equality
* **!=:** unequal
* **lt:** lower than
* **le:** lower or equal
* **gt:** greater than
* **ge:** greater or equal
* **min:** minor value
* **max:** major value
* **complex:** compose complex number

Furthermore there is **rgb** for composing RGB values which is a ternary method.

Each binary operation can combine arrays and/or scalars.
Most scalar-scalar operations are already part of the Scheme programming language.
*AIscm* mostly needs to provide a few numerical operations and some support for RGB and complex values.

```Scheme
@../tests/integration/scalars.scm@
```

One can use an array-scalar operation to divide each colour channels of an image by a number.

![divided.jpg](divided.jpg "Divided image")

```Scheme
@../tests/integration/division.scm@
```

Another example is using the modulo operator to show the remainder of division by an integer for each channel.

![modulo.jpg](modulo.jpg "Remainder values")

```Scheme
@../tests/integration/modulo.scm@
```

Each binary operation can appear in scalar-array, array-scalar, or array-array form.
Also note that the arrays can have different number of dimensions as long as the tail of the shape matches.

```Scheme
@../tests/integration/binary.scm@
```

## Tensor operations

Tensor notation is a compact way to describe operations involving multi-dimensional arrays.
The *tensor* macro is used to declare a tensor expression. *dim* and *get* can be used to create indices and access array elements.
*sum*, *prod*, *largest*, and *smallest* can be used to perform a tensor sum, a cumulative product, or finding the largest/smallest element for a given tensor index.
See examples below for the behaviour of tensors.

```Scheme
@../tests/integration/tensors.scm@
```
