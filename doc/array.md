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

The *fill* method can be used to create an array where the elements are initialised:

```Scheme
@../tests/integration/fill.scm@
```

Index arrays can be used to generate x- and y-ramp arrays:

```Scheme
@../tests/integration/index.scm@
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

### Conversion from/to lists

Scheme list objects can be converted to uniform arrays and vice versa using the methods *to-array* and *to-list*:

```Scheme
@../tests/integration/to_list.scm@
```

*to-array* uses type matching to determine the most suitable type.

## Dimension, shape, and strides

The *dimension* is the number of array indices used to select an element. The *shape* is a list specifying the size of the array in each direction. The *stride* specifies the internal memory layout of the array.

```Scheme
@../tests/integration/array_shape.scm@
```

The array *size* denotes the number of elements while *size-of* tells the storage size of the array. The *get* method can be used to extract elements or array slices.

## Boolean arrays

Boolean arrays are used to store *true* and *false* values. They can be converted to integer arrays and back if required.

```Scheme
@../tests/integration/boolean_array.scm@
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

![pavillion.jpg](pavillion.jpg "Test input image")

... rolling the dimensions will result in the following image:

![rolled.jph](rolled.jpg "Dimension rolled")

```Scheme
@../tests/integration/roll_image.scm@
```

*roll* and *unroll* cycle the dimensions of the array around. Here is and example with a 3D array:

```Scheme
@../tests/integration/roll_unroll.scm@
```

The *project* method can be used to extract the first slice of an array.

## Cropping arrays and dumping elements

One can *dump* array slices from the beginning of the array and *crop* the length of the array, i.e. removing slices from the end of the array.

![cropped.jpg](cropped.jpg "Cropped image")

```Scheme
@../tests/integration/crop_dump.scm@
```

The *dump* and *crop* command can also take a list of values in order to extract a part of a multi-dimensional array:

![crop2d.jpg](crop2d.jpg "2d cropped image")

```Scheme
@../tests/integration/crop_2d.scm@
```

## RGB values

The *rgb* method can be used to combine colour values and images. The following program swaps the colour channels around:

![swap-channels.jpg](swap-channels.jpg "Image with colour channels swapped")

```Scheme
@../tests/integration/swap_channels.scm@
```

One can convert a stereoscopic image into a red-cyan anaglyph image for 3D viewing as shown below.

![shuttle.jpg](shuttle.jpg "Space shuttle stereoscopic image")

```Scheme
@../tests/integration/anaglyph.scm@
```

## Complex values

Here is a small example using complex arrays:

```Scheme
@../tests/integration/complex_array.scm@
```

Since native integers are used, numerical overflow can occur.
Note that you can use *to-type* to convert an array to a more suitable type.
