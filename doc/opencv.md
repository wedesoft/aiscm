# OpenCV bindings
## Connected components

The following example performs connected component analysis.

![components.png](components.png "Connected Components")

```Scheme
@../tests/integration/components.scm@
```

## Charuco board

The example below shows how to use OpenCV to detect a Charuco board.

```Scheme
@../tests/integration/charuco.scm@
```

A Charuco board image can be generated as follows.

```Scheme
@../tests/integration/charuco-board.scm@
```

![board.png](board.png "Charuco board")

A printed Charuco board can be used to calibrate a camera as shown below.

```Scheme
@../tests/integration/calibrate_camera.scm@
```
