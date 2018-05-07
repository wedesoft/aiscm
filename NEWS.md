# aiscm (0.15.2)

* FFMpeg 4.0.0 compatibility fixes

# aiscm (0.15.1)

* Update dependency of Debian package to Guile-2.2
* Update standards version of Debian package

# aiscm (0.14.1)

* Refactored implementation of "mov"
* use intermediate result when MOVSX and MOVZX encounter pointer target
* "set" method for setting subarrays
* "get" method for getting subarrays
* X.Org fullscreen windows
* move and resize windows programmatically

# aiscm (0.13.1)

* n-dimensional convolutions
* refactor code for delegating operations to machine code
* selection of values using 'where'
* rename >=, >, <=, < to ge, gt, le, lt

# aiscm (0.12.1)

* tensor operations
* improved modularity of JIT compiler

# aiscm (0.11.2)

* aiscm/ffmpeg-helpers-test.c: fixed Debian sbuild warning "array subscript is above array bounds"

# aiscm (0.11.1)

* aiscm/ffmpeg.scm: write video and audio files using FFmpeg

# aiscm (0.10.2)

* aiscm/jit.scm: Bugfix for zero-expanding 32-bit integer


# aiscm (0.10.1)

* aiscm/jit.scm: switch to faster linear scan register allocator
* install CSS style sheets and some icons with HTML documentation


# aiscm (0.9.2)

* test/test_ffmpeg.scm: extract pixel value earlier in test
* aiscm/jit.scm: compile code to allocate return values
* support for native constants
* working on tensor expressions


# aiscm (0.9.1)

* aiscm/jit.scm: compile code to allocate return values
* support for native constants
* working on tensor expressions


# aiscm (0.8.3)

* object rgb and object complex values
* refactored native method calls
* improved docker build


# aiscm (0.8.2)

* temporarily match floating point numbers to objects


# aiscm (0.8.1)

* refactored type conversions
* type conversions of method arguments


# aiscm (0.7.1)

* doc/installation.md: updated installation documentation
* aiscm/aiscm.xpm: added installation documentation
* FFmpeg network stream example
* better test video
* use re-export statements to make modules more independent
* use read-image, read-audio, write-image, and write-audio throughout
* renamed "match" to "native-type"


# aiscm (0.6.2)

* Make releases for different distros


# aiscm (0.6.2)

* fix package dependencies (do not use aliases)


# aiscm (0.6.1)

* Pulse audio input and output using asynchronous API
* ring buffer for first-in first-out (FIFO) buffering of data
* improved documentation


# aiscm (0.5.1)

* aiscm/ffmpeg.scm: reading video/audio files using FFmpeg
* aiscm/pulse.c: initialise "error" integer to PA_OK
* aiscm/util.scm: synchronisation method for audio-video-synchronisation
* aiscm/v4l2.c: refactor video capture code a bit
* Dockerfile: Docker build for Debian Jessie/Sid, Ubuntu Trusty/Xenial
* aiscm/xorg.scm: use XVideo as default for single-window videos
* updated documentation


# aiscm (0.4.2)

* compose RGB and complex values from arrays
* added some documentation
* aiscm/jit.scm: refactored dispatching code


# aiscm (0.4.1)

* refactored jit compiler
* aiscm/jit.scm: tensor implementation (WIP)


# aiscm (0.3.1)

* aiscm/asm.scm: support for CMOVcc
* aiscm/jit.scm: major and minor number, =0, !=0, &&, ||
* aiscm/complex.scm: support for complex numbers
* aiscm/magick.scm: loading and saving of images via ImageMagick
* aiscm/pulse.scm: Pulse audio I/O
* n-ary operations &&, ||


# aiscm (0.2.3)

* aiscm/jit.scm: refactored code
* aiscm/asm.scm: support for AH, CH, DH, and BH
* aiscm/xorg.scm: display lists of images and videos with multiple channels
* wrap variables in order to support boolean
* integer remainder of division
* RGB operations


# aiscm (0.2.2)

* aiscm/xorg.scm: convert array to image before displaying
* aiscm/rgb.scm: support for RGB values
* aiscm/jit.scm: spill predefined registers if they are blocked
* changed garbage collector settings for benchmark


# aiscm (0.2.1)

* aiscm/jit.scm: code fragments
* aiscm/op.scm: array operators based on code fragments
* block regions for reserving registers
* integer division
* boolean operations


# aiscm (0.1.8)

* IDIV and DIV using blocked registers
* Shortcuts 'seq' and 'arr'
* Boolean operations for arrays
* Added benchmark


# aiscm (0.1.7)

* Packaging new version


# aiscm (0.1.6)

* Initial release.
* Updated dependencies.
