# Array convolutions
## One-dimensional convolutions

One dimensional signals such as audio samples can be filtered using a 1D convolution.
Convolution can also be performed using composite values such as complex numbers.

```Scheme
@../tests/integration/convolution_1d.scm@
```

## Two-dimensional convolutions

Convolution can also be performed on 2D data.

```Scheme
@../tests/integration/convolution_2d.scm@
```

## Image processing

Here is the test input image for comparison.

![star-ferry.jpg](star-ferry.jpg "Test input image")

### Box filter

A simple filter for blurring an image is the box filter. Note that convolution is performed on a colour image.

```Scheme
@../tests/integration/box_filter.scm@
```

![box-filter.jpg](box-filter.jpg "Box blur filter")

### Edge detection

Convolutions can be used for edge detection.

Here is an implementation of the Roberts cross edge detector.

```Scheme
@../tests/integration/roberts_cross.scm@
```

![roberts-cross.jpg](roberts-cross.jpg "Roberts cross edge detector")

Another popular edge detector is the Sobel operator.

```Scheme
@../tests/integration/sobel.scm@
```

![sobel.jpg](sobel.jpg "Sobel edges")
