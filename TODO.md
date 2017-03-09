## Ready

* intermediate/packed audio frame
* conversion to target type (and planar if necessary)
* encode audio
* refactor list\_timestamped\_audio
* refactor import-audio-frame (return samples instead of array)
* use libavresample if libswresample is not present
```
https://autotools.io/pkgconfig/pkg_check_modules.html
PKG_CHECK_MODULES([UDEV], [libudev],
   [AC_DEFINE([HAVE_UDEV], [1], [Use UDEV])],
      [PKG_CHECK_MODULES([HAL], [hal],
             [AC_DEFINE([HAVE_HAL], [1], [Use HAL])
                 ])
      ])
```
* image-helpers.c -> util-helpers.c
* replace pack\_audio?
* pulse audio: reexport pointer stuff
* module libraries without version numbers
* allocate audio buffer
* record audio
* record audio/video
* run integration tests in Docker container
* negotiate sampling rate, channels, buffer size
* open-video-output, open-audio-output, ...
* check audio/video frame can be written
* tensor sum
* Debian sid FFmpeg API changes
* fix dts error for mp4 output (Ubuntu Trusty)
* select video codec, select audio codec
* video player with aspect ratio
* use -module -shared -avoid-version
* integration tests for XVideo, OpenGL, and XImage
* fast live analysis
* parameter passing for sequences, map, tensor operations, replace fill and other?
    (accessors s) -> ((pointer stride count) ...) which pointer?
    (tensor [i] ((roll m) i))
    (tensor [i] ((roll (+ m s) i))
    (tensor [i] (get m i 1))
    (tensor [i j] (* (s i) (s j)))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
    (tensor [i j] (* (s i) (s j)))
* GNOME video widget
* uscan --report-status; gpg --armor --output ... --detach-sig ...
* NArray-like reshaping, other narray stuff
* diagonal injection, convolution, median, dilation, ...
* floating point numbers (2.3.5: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps), floor, ceil, round
* floating point exception (SIGFPE)
* record arrays, extendable type matching, map?, inject?
* fractions, \*\*
* separate function for defining operation for array?
* separate module for register allocation
* Lee - Debunking the 100X GPU vs. CPU myth: an evaluation of throughput computing on CPU and GPU
  multi-threading, SIMD, memory access (cache)
* robot speech processing
* add checks https://wiki.debian.org/HowToPackageForDebian, blhc, licensecheck
* make project more self contained (remove web streaming integration test)
* define operators directly: (+ <int> <int>) -> (lambda (out . args) ....)
* car, cadr, ... -> first, second, ...
* conditional selection using boolean array
* index manipulation on function expressions
* direct mapping instead of decompose-arg for rgb and complex?
* document motivation/rationale: vector instructions, compose array functions and calls, see tensor paper
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* add docstrings, REPL docs: ,d ...
  http://www.draketo.de/proj/guile-basics/
* https://www.gnu.org/software/guile/manual/html\_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* red, green, blue, real-part, imag-part for objects
* convert rgb to object, convert complex to object
* convert object to rgb, convert object to complex
* 256 -> boolean conversion
* 32 -> boolean conversion
* fast fill method? fast set method
* split up (aiscm jit)
* line-by-line delayed video
* smeared video (weighted average of previous value and current frame)
* call needs to keep variables live, generate code for shuffling variables, use RAX as intermediate if required, use "predefined" as hints
* ArUco markers robot control
* fftw3, hypercomplex, kinect, linalg, opencv, qt4
* predator tracker
* <-> Guile 6.7.5 Arrays
* create relocatable code and use linker to insert calls with relative addresses
* use C parser to get method types? C++ binary interace? Qt bindings?
* use (ice-9 match) to simplify and translate assembler code?
  https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html
* reading and writing with Guile file decriptors or Guile network sockets?
* Conways way of life in one line of code
* command-line tools, GUI tools (GNOME, GLADE): calibration, tracking, detection, speech processing, SLAM (hash-bang?)
* keywords: data structures,digital signal processing,tensors,input/output (check standard theosaurus)
* deduct sample time from delay in ffplay.scm
* threads for reading and decoding audio/video (also requires caching code), etc/threads.scm
* use ice-9 q? background decoder thread
* synchronise with display to prevent video tearing
* array stack/unstack/record arrays
* https://github.com/antoniogarro/guile-matrix
* test for planar/packed audio (see decoding\_encoding.c)
  http://kodi.wiki/view/Samples
* use libswresample for audio conversions
* split up into separate packages
* inject: equality, sum, mininum, maximum of frames
* shape of xorg window
* xorg window set size, fullscreen windows
* use hash tables
* use assp http://www.scheme.com/csug7/objects.html
* conditional/select
* upsampling (and documentation about up- and downsampling)
* something like Ruby's Struct
* complex abs (magnitude), complex arg (and real)
* video -> panorama stitching (multiresolution)
* web server
* save/load 16 and 32 bit images
* (arr (rgb 1 2 3)); (arr (integer 32 signed) 2 3 4)
* lexical sort
* AdaBoost, decision trees, random forests
* distance transform, Chamfer matching
* SLAM, inverse depth images
* camera calibration
* marker recognition
* x11 display: error handling for 'show', timing
* separate register allocation for better testing (<-> RSpec, Clojure Midje?),
  copy blocked predefined variables instead of spilling them
* duplicate image
* types composed of Scheme objects
* xorg: fast OpenGL rendering of grayscale images (glDrawPixels)
* subpixel corner detector using steerable filters
* use thumbnail images to illustrate examples
* update live intervals instead of recomputing
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* RET keeps return value and callee-saved stuff alive
* weird error when including sequence but not pointer
* slimv, tcp/ip, attach
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* wisp (srfi-119), curly infix (srfi-105), guile --language=wisp <file>
