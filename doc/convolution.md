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

### Edge detection

Convolutions can be used for edge detection.

Here is an implementation of the Roberts cross edge detector.

![star-ferry.jpg](star-ferry.jpg "Test input image")

```Scheme
@../tests/integration/roberts_cross.scm@
```

![roberts-cross.jpg](roberts-cross.jpg "Roberts cross edge detector")

Another popular edge detector is the Sobel operator.

```Scheme
@../tests/integration/sobel.scm@
```

![sobel.jpg](sobel.jpg "Sobel edges")
