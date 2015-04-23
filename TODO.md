# TODO

* spill if live in the beginning!
* do not spill predefined / write spilled register parameters
* function for assigning stack locations to spilled variables
  http://www.cs.cornell.edu/courses/cs412/2008sp/lectures/lec33.pdf
* better test-suite, mocking?
* 1d-array plus 2d-array
* coalesce registers (see Chaitin's paper)
* flatten-code needed?
* increase RSP if using more than 128 bytes
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
* logging?
* nicer website
* element-wise type conversions
* getting ranges from array
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* test code examples
* scm_display(scm_target, scm_current_output_port()); printf("\n");
* reduce number of files to include
* use first, second, ... from srfi-1
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
