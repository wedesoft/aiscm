## Ready

* tensor dimensions (compiled returns list to build array object)
* spill blocked registers into other register
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
* fftw3, hypercomplex, kinect, linalg, opencv, qt4
* predator tracker
* <-> Guile 6.7.5 Arrays
* floating point exception (SIGFPE)
* create relocatable code and use linker to insert calls with relative addresses
* use C parser to get method types? C++ binary interace? Qt bindings?
* use (ice-9 match) to simplify and translate assembler code?
* reading and writing with Guile file decriptors or Guile network sockets?
* ffmpeg output (writing videos)
* Conways way of life in one line of code
* command-line tools, GUI tools (GNOME, GLADE): calibration, tracking, detection, speech processing, SLAM (hash-bang?)
* floating point numbers (2.3.5: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps), floor, ceil, round
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
* parameter passing for sequences, map, tensor operations, replace fill and other?
    (accessors s) -> ((pointer stride count) ...) which pointer?
    (tensor [i] ((roll m) i))
    (tensor [i] ((roll (+ m s) i))
    (tensor [i] (get m i 1))
    (tensor [i j] (* (s i) (s j)))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
    (tensor [i j] (* (s i) (s j)))
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
* subpixel corner detector using steerable filters
* use thumbnail images to illustrate examples
* update live intervals instead of recomputing
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* RET keeps return value and callee-saved stuff alive
* weird error when including sequence but not pointer

## Planned

* slimv, tcp/ip, attach
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* wisp (srfi-119), curly infix (srfi-105), guile --language=wisp <file>
