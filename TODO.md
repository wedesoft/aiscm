## Ready

* conjugate of real number
* major/minor
* real-part, red, ... <element> <-> <sequence<>> <-> <node>/<scalar>
* something like Ruby's Struct
* comoile C function calls (also support SCM values)
* <int> + <int> -> always compile, <object> + <object> -> compile call back into Scheme interpreter
* tensor dimensions
* parameter passing for sequences, map, tensor operations, replace fill and other?
    (accessors s) -> ((pointer stride count) ...) which pointer?
    (tensor [i] ((roll m) i))
    (tensor [i] ((roll (+ m s) i))
    (tensor [i] (get m i 1))
    (tensor [i j] (* (s i) (s j)))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
    (tensor [i j] (* (s i) (s j)))
* use trs files to run failed tests, count tests automatically
* Theano expression compiler: http://www.iro.umontreal.ca/~lisa/pointeurs/theano_scipy2010.pdf
* floating point numbers (2.3.5: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps), floor, ceil, round
* complex abs (magnitude), complex arg (and real)
* X.Org window with width and height?
* wisp (srfi-119), curly infix (srfi-105), guile --language=wisp <file>
* save/load 16 and 32 bit images
* (arr (rgb 1 2 3))
* lexical sort
* NArray-like reshaping
* diagonal injection, convolution, median, dilation, ...
* separate function for defining operation for array?
* imaginary part of variable
* (complex 2 3), (complex (seq 1 2 3) 4)
* (rgb (seq 1 2) 3 (seq 5 6))
* (- (rgb frag frag frag)) -> (frag rgb)???
* record arrays, extendable type matching, map?, inject?
* fractions, \*\*
* x11 display: error handling for 'show', timing
* separate register allocation for better testing (<-> RSpec, Clojure Midje?),
  copy blocked predefined variables instead of spilling them
* duplicate image
* types composed of Scheme objects
* xorg: fast OpenGL rendering of grayscale images (glDrawPixels)
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
* use thumbnail images to illustrate examples
* update live intervals instead of recomputing
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* RET keeps return value and callee-saved stuff alive
* weird error when including sequence but not pointer

## Planned

* <-> Guile 6.7.5 Arrays
* coalesce variables
* floating point exception (SIGFPE)
* matching for lists?
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* REPL docs: (help ...)
* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* https://www.gnu.org/software/guile/manual/html_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* array stack/unstack/record arrays
* slimv, tcp/ip, attach
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* ffmpeg input, fftw3, hypercomplex, kinect, linalg, opencv, qt4
