# Arrays
## Array construction

Arrays can be instantiated using a type specification. Note that the elements are not guaranteed to be initialised:

```Scheme
@../tests/integration/byte_sequence.scm@
```

Uniform arrays can also be created from values using type matching:

```Scheme
@../tests/integration/type_matching.scm@
```

## Multi-dimensional arrays

It is also possible to instantiate multi-dimensional arrays. Again elements are not guaranteed to be initialised:

```Scheme
@../tests/integration/2d_array.scm@
```

Uniform multi-dimensional arrays can also be created from values using type matching:

```Scheme
@../tests/integration/2d_matching.scm@
```

## Integer types

It is also possible to specify the array type when creating an array from values:

```Scheme
@../tests/integration/typed_sequence.scm@
```

Note that the integer type can be specified using number of bits and signed-ness instead:

```Scheme
@../tests/integration/integer_type.scm@
```

## Rolling dimensions

Given the following image ...

![](pavillion.jpg "Test input image")

... rolling the dimensions will result in the following image:

![](rolled.jpg "Dimension rolled")

```Scheme
@../tests/integration/roll_image.scm@
```

## RGB values
### Swap colour channels

The *rgb* method can be used to combine colour values and images. The following program swaps the colour channels around:

![](swap-channels.jpg "Image with colour channels swapped")

```Scheme
@../tests/integration/swap_channels.scm@
```
