# Input/output
## Loading/saving images using ImageMagick

An image can be read from disk using *read-image* which uses the ImageMagick library to load the image.

![](fubk.png "Test input image")

```Scheme
@../tests/integration/read_image.scm@
```

Analogous *write-image* writes an image to disk.

```Scheme
@../tests/integration/write_image.scm@
```

![](scaled-pattern.png "Output image")

## Xorg display
### Images

As shown above, you can display images using the method *show* as follows:

```Scheme
@../tests/integration/read_image.scm@
```

One can also display a list of images:

```Scheme
@../tests/integration/xorg_image_list.scm@
```

### Videos

It is also possible to display a video using the *show* method:

```Scheme
@../tests/integration/xorg_video.scm@
```

A function returning lists of images can be used to display multiple videos synchronously.

```Scheme
@../tests/integration/xorg_video_list.scm@
```

If necessary, one can also handle the display and window objects directly.
Possible types of output are *IO-XIMAGE*, *IO-OPENGL*, and *IO-XVIDEO*.

```Scheme
@../tests/integration/xorg_window.scm@
```

## Video for Linux version 2 (V4L2)

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

Audio data can be recorded in a similar fashion

```Scheme
@../tests/integration/pulse_in.scm@
```

## Video files

The following example shows how to use the FFmpeg interface to open and view a video. The video presentation time stamps are used to display the video at the correct speed.

```Scheme
@../tests/integration/ffvideo.scm@
```

## Audio files

One can play samples from an audio file by passing them to the audio device using the *write-samples* method.
It is also possible to pass a function returning consecutive audio samples as shown below.

```Scheme
@../tests/integration/ffaudio.scm@
```
