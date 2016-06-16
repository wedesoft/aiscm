# Array operations
## Swap colour channels

As shown in the [I/O documentation](io.html "input/output"), one can read an image as follows:

![](fubk.png "Test input image")

```Scheme
@../tests/integration_read_image.scm@
```

The *rgb* method can be used to combine colour values and images. The following program swaps the red and blue channel.

![](swap-channels.png "Image with colour channels swapped")

```Scheme
@../tests/integration_swap_channels.scm@
```
