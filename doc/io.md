# Input/output

## Video for Linux version 2 (V4L2)

You can open a camera and grab a frame as follows.

```Scheme
@../tests/integration_grab.scm@
```

It is also possible to specify the device, a channel, and a closure for selecting the video mode.

```Scheme
@../tests/integration_camera_mode.scm@
```

## Xorg display

You can capture an image from a camera and display it using *show* as follows:

```Scheme
@../tests/integration_xorg_image.scm@
```

One can also display a list of images:

```Scheme
@../tests/integration_xorg_image_list.scm@
```

It is also possible to display a video using the *show* command:

```Scheme
@../tests/integration_xorg_video.scm@
```

A function returning lists of images can be used to display multiple videos synchronously.

```Scheme
@../tests/integration_xorg_video_list.scm@
```

If necessary, one can also handle the display and window objects directly.
Possible types of output are *IO-XIMAGE*, *IO-OPENGL*, and *IO-XVIDEO*.

```Scheme
@../tests/integration_xorg_window.scm@
```

## Loading/saving images using ImageMagick

An image can be read from disk using *read-image* which uses the ImageMagick library to load the image.

```Scheme
@../tests/integration_read_image.scm@
```

Analogous *write-image* writes an image to disk.

```Scheme
@../tests/integration_write_image.scm@
```

## Pulse audio

The following example program creates a sine wave and outputs it to the audio device.

```Scheme
@../tests/integration_pulse_out.scm@
```

Audio data can be recorded in a similar fashion

```Scheme
@../tests/integration_pulse_in.scm@
```
