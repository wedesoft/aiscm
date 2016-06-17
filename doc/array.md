# Arrays
## Array construction

Arrays can be instantiated using a type specification. Note that the elements are not guaranteed to be initialised:

```Scheme
@../tests/integration_byte_sequence.scm@
```

Uniform arrays can also be created from values using type matching:

```Scheme
@../tests/integration_type_matching.scm@
```

## Multi-dimensional arrays

It is also possible to instantiate multi-dimensional arrays. Again elements are not guaranteed to be initialised:

```Scheme
@../tests/integration_2d_array.scm@
```

Uniform multi-dimensional arrays can also be created from values using type matching:

```Scheme
@../tests/integration_2d_matching.scm@
```

## Integer types

It is also possible to specify the array type when creating an array from values:

```Scheme
@../tests/integration_typed_sequence.scm@
```

Note that the integer type can be specified using number of bits and signed-ness instead:

```Scheme
@../tests/integration_integer_type.scm@
```

## RGB values
### Swap colour channels

Given the following image ...

![](pavillion.jpg "Test input image")

... the *rgb* method can be used to combine colour values and images. The following program swaps the colour channels around:

![](swap-channels.jpg "Image with colour channels swapped")

```Scheme
@../tests/integration_swap_channels.scm@
```
