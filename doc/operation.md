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

*AIscm* has a tensor implementation with flexible indexing.

```Scheme
@../tests/integration/tensor.scm@
```

## Warps

Using the warp operation one can perform multi-dimensional warps.
Here is a simple example performing a lookup in a pseudo-color table.

![pseudo.jpg](pseudo.jpg "Pseudo-color lookup")

```Scheme
@../tests/integration/pseudo.scm@
```

A warp can also be used to mirror an array.
In this case index arrays are used to define a warp field.

![mirror.jpg](mirror.jpg "Mirroring an array")

```Scheme
@../tests/integration/mirror.scm@
```

## Histograms

One can compute histograms of one or more arrays of coordinates.
The following example creates a circle.

![circle.png](circle.png "Circle")

```Scheme
@../tests/integration/circle.scm@
```
