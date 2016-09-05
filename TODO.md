## Ready

* nesting function calls
  prepare-parameter, insert-intermediate, need-intermediate-param?
* packages for sid, xenial -> installation documentation
* stack parameters
* use Sintel video extract (or compressed video or bunny video)
* jit calls to other already compiled methods
* spill blocked registers into other register
* avoid multiple copy operations (block boundaries?)
* tensor dimensions
* closing of ffmpeg input, ffmpeg output
* floating point numbers (2.3.5: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps), floor, ceil, round
* command-line tools, GUI tools: calibration, tracking, detection, speech processing, SLAM
* keywords: computer vision,image processing for vision,data structures,digital signal processing,tensors
* deduct sample time from delay in ffplay.scm
* threads for reading and decoding audio/video (also requires caching code), etc/threads.scm
* use ice-9 q? background decoder thread
* synchronise with display to prevent video tearing
* writing videos
* https://github.com/antoniogarro/guile-matrix
* test data license https://www.youtube.com/watch?v=cGgf_dbDMsw
* test for planar/packed audio (see decoding\_encoding.c)
  http://kodi.wiki/view/Samples
* use libswresample for audio conversions
* add network streaming example http://peach.themazzone.com/durian/movies/sintel-1024-surround.mp4 in examples,
* split up into separate packages
* inject: equality, sum, mininum, maximum of frames
* basic io module, read-video -> read-image, v4l2: grab -> read-image
* shape of xorg window
* xorg window scaling, fullscreen windows
* use hash tables
* use assp http://www.scheme.com/csug7/objects.html
* conditional/select
* git-init.xml asciidoc? jadetex? docbook2x
* upsampling (and documentation about up- and downsampling)
* something like Ruby's Struct
* <int> + <int> -> always compile, <object> + <object> -> compile call back into Scheme interpreter
* parameter passing for sequences, map, tensor operations, replace fill and other?
    (accessors s) -> ((pointer stride count) ...) which pointer?
    (tensor [i] ((roll m) i))
    (tensor [i] ((roll (+ m s) i))
    (tensor [i] (get m i 1))
    (tensor [i j] (* (s i) (s j)))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
    (tensor [i j] (* (s i) (s j)))
* complex abs (magnitude), complex arg (and real)
* web server
* X.Org window with width and height?
* save/load 16 and 32 bit images
* (arr (rgb 1 2 3)); (arr (integer 32 signed) 2 3 4)
* lexical sort
* AdaBoost, decision trees, random forests
* distance transform, Chamfer matching
* SLAM, inverse depth images
* camera calibration
* marker recognition
* NArray-like reshaping, other narray stuff
* diagonal injection, convolution, median, dilation, ...
* separate function for defining operation for array?
* record arrays, extendable type matching, map?, inject?
* fractions, \*\*
* x11 display: error handling for 'show', timing
* separate register allocation for better testing (<-> RSpec, Clojure Midje?),
  copy blocked predefined variables instead of spilling them
* duplicate image
* types composed of Scheme objects
* xorg: fast OpenGL rendering of grayscale images (glDrawPixels)
* use thumbnail images to illustrate examples
* update live intervals instead of recomputing
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* RET keeps return value and callee-saved stuff alive
* weird error when including sequence but not pointer

## Planned

* <-> Guile 6.7.5 Arrays
* floating point exception (SIGFPE)
* matching for lists?
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* REPL docs: (help ...)
* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* https://www.gnu.org/software/guile/manual/html\_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* array stack/unstack/record arrays
* slimv, tcp/ip, attach
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* ffmpeg input, fftw3, hypercomplex, kinect, linalg, opencv, qt4
* wisp (srfi-119), curly infix (srfi-105), guile --language=wisp <file>
