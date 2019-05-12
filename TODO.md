## Ready

* Tensorflow strings: tf-identity, tf-string-join, tf-string-split
* how to broadcast vectors
* Ghost in the Shell style animations (coins?)
* histogram with weights and customisable default and reduce op (e.g. min instead of + to find bounding box for each component)
* Debian sid FFmpeg API changes
  * avcodec\_encode\_video2 deprecated -> avcodec\_send\_frame and avcodec\_receive\_packet
  * avcodec\_decode\_video2 deprecated
  * avcodec\_encode\_audio2 deprecated
  * avcodec\_decode\_audio4 deprecated
  * https://github.com/pesintta/vdr-plugin-vaapidevice/issues/32
    * avcodec\_receive\_frame
    * avcodec\_send\_packet
  * codec deprecated (ffmpeg.c: 271, 377, 417)
* check new lintian tag required
* to-array -> from-image
* argmin, argmax
* mask, unmask
* argwhere (mask coordinates)
* Kinect bindings
* FFTW3 bindings, replace FFT in audio spectrum example
* Lucas-Kanade tracker
* OpenGL vertex array object example, RGB arrays, vertex arrays, texture images
* port using qemu
* multiple C function calls in llvm-typed environment
* how to link C functions in LLVM
* For Scheme objects: unary !, binary &&, ||, lt, le, gt, ge, eq, ne
* RGB to object, object to RGB
* runge kutta with state vector
* 3ds converter computing normals and tangentials and loading textures and maps
* micro-collisions example
* type conversions (e.g. scalar to complex, scalar to rgb)
* use thumbnail images to illustrate examples
* panorama video viewer
* to-type in tensor expressions, implicit typecasting in tensor sum?
* cast to higher integer type when doing a tensor injection
* rigid-body impulse-based dynamics example (and microcollisions)
* function should return list, sequential computation of complex number
* compile method calls to other (polymorphic) compiled methods (support modular JIT code)
* polymorphic method calls (e.g. fabs, fabsf)
* colour swapped when converting MJPEG video to I420 (but not YV12)
* 2D-1D convolution, 2D-3D convolution (Sobel pair), 0D-0D convolution
* ==, != for complex numbers
* galaxy simulation
* floating point exception (SIGFPE)
* linear algebra bindings (blas, lapack, gsl, mkl)
* convolve with set of sobel filters
* sum with multiple indices or no indices, standalone sum expression
* inject: equality, sum, minimum, maximum of frames
* pixel selection example (take reference from center pixels)
* audio frequency: bar analyser
* GNOME video widget and player, Python/Qt/PyQt integration
* how to run test suite
* documentation for objects and arrays of objects
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
* red, green, blue, real-part, imag-part for objects
* convert rgb to object, convert complex to object
* convert object to rgb, convert object to complex
* line-by-line delayed video
* smeared video (weighted average of previous value and current frame)
* hypercomplex numbers and quaternions
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
* save/load 16 and 32 bit images
* (arr (rgb 1 2 3)); (arr (integer 32 signed) 2 3 4)
* lexical sort
* AdaBoost, decision trees, random forests
* distance transform, Chamfer matching
  http://www.gavrila.net/Research/Chamfer_System/chamfer_system.html
* SLAM, inverse depth images
* camera calibration
* x11 display: error handling for 'show', timing
* xorg: fast OpenGL rendering of grayscale images (glDrawPixels)
* subpixel corner detector using steerable filters
