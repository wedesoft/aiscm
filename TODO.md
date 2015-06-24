# TODO

* element-wise equal
  copying binary op
  type conversion for binary op
* DIV, IDIV, CWD, CDQ, CQO
* complete coverage of operations
* monkey patching? Guile reader? duplicate binding handler?
* separate assembler
* linear-scan register allocator
* reader (e.g. #vu8(1 2))
* better reporting in test suite (expect ... to be)
* tensor operations
* floating point numbers
* RET keeps return value and callee-saved stuff alive (EAX is predefined variable?)
* matching for lists?
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* REPL docs: (help ...)
* GOOPS monkey patching
* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* https://www.gnu.org/software/guile/manual/html_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* array stack/unstack/record arrays
* RGB, libswscale2, libmjpegtools (mjpegtools/jpegutils.h -> decode_jpeg_raw)
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
