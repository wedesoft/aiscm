# TODO

* (+ (seq 10000000000000000000) (seq 9999999))
* floating point exception
* spill predefined variable if blocked?
* code fragments: code, return value(s), predefined (i/o of CDQ? RET?)
* coalesce variables
* separate assembler
* RET keeps return value and callee-saved stuff alive (EAX is predefined variable?)
  (e.g. (block EAX (MOV r x)) (RET))
* monkey patching? duplicate binding handler?
  #:replace, #:duplicates
* linear-scan register allocator/block-wise coloring
* better reporting in test suite (expect ... to be)
* tensor operations
* seq.int, arr.int, to-int, to-list
* floating point numbers
* minor, major (cmovge)
* RGB
* matching for lists?
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* REPL docs: (help ...)
* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* https://www.gnu.org/software/guile/manual/html_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* array stack/unstack/record arrays
* slimv
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
* ffmpeg input, fftw3, pulse-audio, hypercomplex, kinect, linalg, opencv, qt4, rmagick
