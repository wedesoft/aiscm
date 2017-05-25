## Ready

* complex max, min, max=, min=
* fix delegate-op hack for cumulative operations
* simplify RGB and complex cumulative operation definition
* operation-code list of args -> args
* to-type in tensor expresssions, implicit typecasting in tensor sum?
* n-dimensional tensor sums, standalone sum expression
* code -> mov
* cache fill method
* GNOME video widget and player
* check against NumPy for completeness
* (tensor (sum j (get (largest i (get (arr (2 3 5) (8 6 4)) i)) j)))
* move more tests for jit-compile into jit compiler test suite
* put compiler stuff into separate module
* rbg and complex tensors
* sum with multiple indices or no indices
* how to run test suite
* correlation: \*, +=, dilation: first/conditional, max, ...
* array compatibility check
* "let" for tensors
* convolution/correlation in tensor (finite element analysis), separable convolution, 2D-1D convolution
* remove unnecessary copying from injection tensor loop (use +=, \*=, max=, ...)
* documentation for objects and arrays of objects
* cast to higher integer type when doing a tensor injection
* multiple use of one indexer (get s i i) -> merge lookups (tensor contraction)
* (tensor i (roll (get m i)))
* (tensor i (project (get m i)))
* parameter passing for sequences, map, tensor operations, replace fill and other?
    (accessors s) -> ((pointer stride count) ...) which pointer?
    (tensor [i] ((roll m) i))
    (tensor [i] ((roll (+ m s) i))
    (tensor [i] (get m i 1))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
* equality of arrays
* inject: equality, sum, mininum, maximum of frames
* histogram with weights and customisable reduce operation (e.g. min instead of + to find bounding box for each component)
* ffmpeg-input, ffmpeg-output class (instead of "is-input?")
* Docker integration tests
* audio, video output timestamps
* pulse audio: use samples for reading and writing
* pulse audio: reexport pointer stuff
* module libraries without version numbers
* virtual fighter
* augmented reality sudoku solver https://github.com/jponttuset/sudoku
* run integration tests in Docker container
* open-video-output, open-audio-output, ...
* check audio/video frame can be written
* tensor sum
* Debian sid FFmpeg API changes
* fix dts error for mp4 output (Ubuntu Trusty)
* select video codec, select audio codec
* video player with aspect ratio
* integration tests for XVideo, OpenGL, and XImage
* uscan --report-status; gpg --armor --output ... --detach-sig ...
* NArray-like reshaping, other narray stuff
* diagonal injection, convolution, median, dilation, ...
* floating: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps), floor, ceil, round
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
* ArUco markers robot control
* fftw3, hypercomplex, kinect, linalg, opencv, qt4
* predator tracker
* <-> Guile 6.7.5 Arrays
* create relocatable code and use linker to insert calls with relative addresses
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
  http://www.gavrila.net/Research/Chamfer_System/chamfer_system.html
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
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* slimv, tcp/ip, attach
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* wisp (srfi-119), curly infix (srfi-105), guile --language=wisp <file>
