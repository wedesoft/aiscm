## Ready

* OpenGL vertex array object example
* 3DS files
* technical report about rigid body dynamics
* prototype space game
* lookup, pseudo colour, warp
* mask
* argmin, argmax
* connected components analysis
* port using qemu
* rgb &, |, ^, <<, >>
* multiple C function calls in llvm-typed environment
* how to link C functions in LLVM
* For Scheme objects: unary !, binary &&, ||, lt, le, gt, ge, eq, ne
* RGB to object, object to RGB
* runge kutta with state vector
* 3ds converter computing normals and tangentials and loading textures and maps
* micro-collisions example
* type conversions (e.g. scalar to complex, scalar to rgb)
* min/max
* use thumbnail images to illustrate examples
* Ghost in the Shell style animations
* panorama video viewer
* tensor indices
* to-type in tensor expressions, implicit typecasting in tensor sum?
* cast to higher integer type when doing a tensor injection
* multiple use of one indexer (get s i i) -> merge lookups (tensor contraction)
* parameter passing for sequences, map, tensor operations, replace fill and other?
* (tensor [i] (get m i 1)) (tensor i (project (get m i))) (tensor i (roll (get m i)))
* dimension checks for array/tensor operations; array compatibility check
* Debian sid FFmpeg API changes
  * avcodec\_encode\_video2 deprecated
  * avcodec\_decode\_video2 deprecated
  * avcodec\_encode\_audio2 deprecated
  * avcodec\_decode\_audio4 deprecated
  * codec deprecated (ffmpeg.c: 271, 377, 417)
* matrix vector operations
* rigid-body impulse-based dynamics example (and microcollisions)
* function should return list, sequential computation of complex number
* compile method calls to other (polymorphic) compiled methods (support modular JIT code)
* polymorphic method calls (e.g. fabs, fabsf)
* Lucas-Kanade tracker
* recognise Aruco marker cube
* colour swapped when converting MJPEG video to FFmpeg video
* procedural terrain detail generator (repeatable hashed detail/seeding, wavelet pyramid)
* 2D-1D convolution, 2D-3D convolution (Sobel pair), 0D-0D convolution
* 1+, 1-
* ==, != for complex numbers
* galaxy simulation
* floating point exception (SIGFPE)
* floating point convolution: sharpening filter, corner filters
* linear algebra bindings (blas, lapack, gsl, mkl)
* separable convolution
* convolve with set of sobel filters
* diagonal injection, convolution, median, dilation, ...
* sum with multiple indices or no indices, standalone sum expression
* inject: equality, sum, minimum, maximum of frames
* pixel selection example (take reference from center pixels)
* audio frequency: bar analyser
* GNOME video widget and player, Python/Qt/PyQt integration
* how to run test suite
* documentation for objects and arrays of objects
* histogram with weights and customisable reduce operation (e.g. min instead of + to find bounding box for each component)
* difference picture photo thunderstorm
* virtual fighter
* run integration tests in Docker container
* select video codec, select audio codec
* video player with aspect ratio
* NArray-like reshaping, other narray stuff
* fractions
* multi-threading, SIMD, memory access (cache)
* make project more self contained (remove web streaming integration test)
* index manipulation on function expressions
* document motivation/rationale: vector instructions, compose array functions and calls, see tensor paper
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* red, green, blue, real-part, imag-part for objects
* convert rgb to object, convert complex to object
* convert object to rgb, convert object to complex
* line-by-line delayed video
* smeared video (weighted average of previous value and current frame)
* hypercomplex numbers and quaternions
* Kinect bindings
* OpenCV bindings
* FFTW3 bindings
* predator tracker
* <-> Guile 6.7.5 Arrays
* reading and writing with Guile file decriptors or Guile network sockets?
* command-line tools, GUI tools (GNOME, GLADE): calibration, tracking, detection, speech processing
* threads for reading and decoding audio/video (also requires caching code), etc/threads.scm
* use ice-9 q? background decoder thread
* array stack/unstack/record arrays
* test for planar/packed audio (see decoding\_encoding.c)
  http://kodi.wiki/view/Samples
* upsampling (and documentation about up- and downsampling)
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
* xorg: fast OpenGL rendering of grayscale images (glDrawPixels)
* subpixel corner detector using steerable filters
* red-cyan, 3d display (bino, libglewmx, libavdevice)
