## Ready

* fetch composite values
* complex constant
* pointer types
* refactor llvm-let and prepare-return for complex numbers
* llvm-let (trigger memoization)
* standards version 4.1.3
* function should return list, sequential computation of complex number
* coercion when composing or adding complex numbers
* typed let
* complex operations
* structures/composite values (complex numbers, vectors, quaternions)
* compile method calls to other (polymorphic) compiled methods (support modular JIT code)
* polymorphic method calls (e.g. fabs, fabsf)
* nested structures (e.g. state vector)
* Scheme objects
* pointers and arrays
* type matching for uniform arrays
* dynamic array operations
* boolean basic type
* unary =0, !=0, !, <<, >>
* binary /, %, <<, >>, &, |, ^, &&, ||, ==, !=, lt, ge, gt, ge, min, max, where
* add Guile to dependencies
* OpenGL vertex array object example
* rigid-body impulse-based dynamics example (and microcollisions)
* structs, jit methods with multiple return values, packed objects (e.g. quaternion)
* Lucas-Kanade tracker
* recognise Aruco marker cube, calibrated inverse kinematic
* colour swapped when converting MJPEG video to FFmpeg video
* tensor indices
* procedural terrain detail generator (repeatable hashed detail/seeding, wavelet pyramid)
* Debian sid FFmpeg API changes
  deprecated: avcodec\_encode\_video2, avcodec\_encode\_audio2, av\_free\_packet, codec, pkt\_pts, avcodec\_decode\_audio4, avcodec\_decode\_video2
* 2D-1D convolution, 2D-3D convolution (Sobel pair), 0D-0D convolution
* camera calibration, robot inverse kinematics, robot speech processing
* 1+, 1-
* ==, != for complex numbers
* galaxy simulation
* to-type in tensor
* floating point exception (SIGFPE)
* floating point convolution: sharpening filter, corner filters
* linear algebra bindings (blas, lapack, gsl, mkl)
* separable convolution
* convolve with set of sobel filters
* convolution: \*, +=, dilation: first/conditional, max, ...
* diagonal injection, convolution, median, dilation, ...
* sum with multiple indices or no indices, standalone sum expression
* index arrays, show fill status of glass of milk
* inject: equality, sum, mininum, maximum of frames
* pixel selection example (take reference from center pixels)
* lookup, pseudo colour, warp
* to-type in tensor expressions, implicit typecasting in tensor sum?
* (tensor [i] (get m i 1)) (tensor i (project (get m i))) (tensor i (roll (get m i)))
* dimension checks for array/tensor operations; array compatibility check
* argmax
* Ghost in the Shell style animations
* audio frequency: bar analyser
* GNOME video widget and player, Python/Qt/PyQt integration
* how to run test suite
* documentation for objects and arrays of objects
* cast to higher integer type when doing a tensor injection
* multiple use of one indexer (get s i i) -> merge lookups (tensor contraction)
* parameter passing for sequences, map, tensor operations, replace fill and other?
* equality of arrays
* histogram with weights and customisable reduce operation (e.g. min instead of + to find bounding box for each component)
* ffmpeg-input, ffmpeg-output class (instead of "is-input?")
* Docker integration tests
* audio, video output timestamps
* pulse audio: use samples for reading and writing
* module libraries without version numbers
* Kinect difference picture/surveillance
* virtual fighter
* augmented reality sudoku solver https://github.com/jponttuset/sudoku
* run integration tests in Docker container
* open-video-output, open-audio-output, ...
* check audio/video frame can be written
* select video codec, select audio codec
* video player with aspect ratio
* integration tests for XVideo, OpenGL, and XImage
* NArray-like reshaping, other narray stuff
* record arrays, extendable type matching, map?, inject?
* fractions, \*\*
* separate function for defining operation for array?
* separate module for register allocation
* multi-threading, SIMD, memory access (cache)
* add checks https://wiki.debian.org/HowToPackageForDebian, blhc, licensecheck
* make project more self contained (remove web streaming integration test)
* car, cadr, ... -> first, second, ...
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
* line-by-line delayed video
* smeared video (weighted average of previous value and current frame)
* ArUco markers robot control
* hypercomplex numbers and quaternions
* linalg bindings
* Kinect bindings
* OpenCV bindings
* FFTW3 bindings
* predator tracker
* <-> Guile 6.7.5 Arrays
* create relocatable code and use linker to insert calls with relative addresses
* use (ice-9 match) to simplify and translate assembler code?
  https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html
* reading and writing with Guile file decriptors or Guile network sockets?
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
