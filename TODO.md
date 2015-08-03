# TODO

## Ready

* tests for code-fragment implementation
* integrate fragments with parametrisation code
* pretty printing for <int>, ...
  (param (sequence <int>) (list a b c)) not printable
* use MOV when typecasting to smaller integer
* port (aiscm op) to new jit
* macros: (compile (test (x <int>) (y <float>)) (+ x y))
* map, tensor operations (code fragments: code, return value(s), predefined)
    (tensor [i j] (* (s i) (s j)))
    (tensor [i j] (sum (k) (* ((m i) k) ((m k) j))))
* floating point numbers (2.3.5: VEX prefix, vcvttss2si, vcvtsi2ss, vmovss, vxorps)
* RGB
* red-cyan, 3d display (bino, libglewmx, libavdevice)
* Scheme objects
* RET keeps return value and callee-saved stuff alive
* update live intervals instead of recomputing

## Planned

* faster loading and storing of integers
* <-> Guile 6.7.5 Arrays
* spill predefined variable if blocked? spill into register?
* coalesce variables
* floating point exception (SIGFPE)
* linear-scan register allocator/block-wise coloring
* better reporting in test suite (expect ... to be)
* minor, major (cmovge)
* matching for lists?
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* REPL docs: (help ...)
* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* https://www.gnu.org/software/guile/manual/html_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* array stack/unstack/record arrays
* slimv, tcp/ip, attach
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* ffmpeg input, fftw3, pulse-audio, hypercomplex, kinect, linalg, opencv, qt4, rmagick

(define-syntax-rule (timeit s x)
  (begin (format #t "~a ..." s) (let [(retval x)] (format #t " done~&") retval)))
