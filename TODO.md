# TODO

* increase RSP if using more than 128 bytes; (ptr <int> RSP offset -8)
* benchmark
* RET keeps return value and callee-saved stuff alive (EAX is predefined variable?)
* linear-scan register allocator
* http://www.cs.cornell.edu/courses/cs412/2008sp/lectures/lec33.pdf
* https://www.gnu.org/software/guile/manual/html_node/Arrays.html#Arrays
  http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
* document motivation: vector instructions, compose array functions and calls,
  Alan Kay, Ian Piumarta: mini-languages which step out of the way (instead of compiler black-box)
* make structs of variables printable
* matching for lists?
* array stack/unstack/record arrays
* pack and unpack necessary? instances of <int> and <bool> necessary?
* GOOPS monkey patching
* REPL docs: (help ...)
* RGB, libswscale2, libmjpegtools (mjpegtools/jpegutils.h -> decode_jpeg_raw)
* element-wise type conversions
* logging?
* test code examples
* better test-suite, mocking?
* nicer website
* reduce number of files to include
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
