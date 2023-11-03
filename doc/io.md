# Input/output

The I/O modules provide sources (and sinks) for video and audio data.

## Loading/saving images using ImageMagick

An image can be read from disk using *read-image* which uses the ImageMagick library to load the image.

![fubk.png](fubk.png "Test input image")

```Scheme
@../tests/integration/read_image.scm@
```

Using colourspace conversions one can convert images to gray scale.

```Scheme
@..tests/integration/show_gray.scm
```

*write-image* writes an image to disk.

```Scheme
@../tests/integration/write_image.scm@
```

![scaled-pattern.png](scaled-pattern.png "Output image")

## Window display
### Images

As shown above, you can display images under XOrg using the method *show*:

```Scheme
@../tests/integration/read_image.scm@
```

The size of the window is the size of the image by default.
To override it, one can use the *shape* keyword argument.

```Scheme
@../tests/integration/xorg_scale.scm@
```

One can also display a list of images:

![fubk-colours.png](fubk-colours.png "List of images")

```Scheme
@../tests/integration/xorg_image_list.scm@
```

The *fullscreen* keyword can be used to open a borderless maximized window.

```Scheme
@../tests/integration/xorg_fullscreen.scm@
```

### Videos

It is also possible to display a video using the *show* method:

```Scheme
@../tests/integration/xorg_video.scm@
```

One can use the *shape* keyword argument to customise the window size.

```Scheme
@../tests/integration/xorg_scale_video.scm@
```

A function returning lists of images can be used to display multiple videos synchronously.

The *fullscreen* keyword can be used to display a video in a borderless maximized window.

```Scheme
@../tests/integration/xorg_fullscreen_video.scm@
```

```Scheme
@../tests/integration/xorg_video_list.scm@
```

If necessary, one can also handle the display and window objects directly.
Possible types of output are *IO-XIMAGE*, *IO-OPENGL*, and *IO-XVIDEO*.
The following example shows showing, hiding, moving, resizing of windows.
*process-events* needs to be invoked to process pending events.

```Scheme
@../tests/integration/xorg_window.scm@
```

## Video for Linux version 2 (V4L2)

![v4l2.jpg](v4l2.jpg "V4L2 input image")

As shown above already, you can open a camera and grab a frame as follows.

```Scheme
@../tests/integration/grab.scm@
```

It is also possible to specify the device, a channel, and a closure for selecting the video mode.

```Scheme
@../tests/integration/camera_mode.scm@
```

## Pulse audio

The following example program creates a sine wave and outputs it to the audio device.

```Scheme
@../tests/integration/pulse_out.scm@
```

The *drain* method waits for the content of the audio buffer to finish playing.
The method *flush* (not shown here) can be used to empty the audio buffer.

Audio data can be recorded in a similar fashion.
The following example records 3 seconds of audio data and then plays it back.

```Scheme
@../tests/integration/pulse_in.scm@
```

One can display a block-wise audio spectrum using the *Tensorflow* Fourier transform.

```Scheme
@../tests/integration/spectrum.scm@
```
