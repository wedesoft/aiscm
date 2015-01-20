# TODO

(use-modules (aiscm jit) (aiscm int) (aiscm util) (oop goops) (srfi srfi-1) (srfi srfi-26))
(define my-codes (map get-code (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX RBP R12 R13 R14 R15)))
(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))
(define prog (list (JE 'x) (MOV a 0) 'x (MOV b a) (RET)))
(resolve-jumps (subst prog (color-graph (collisions prog) my-codes '())))
; put into method, test, test with predefined return register, test with predefined parameter register

(rtl [(a (make <var> #:type <int>)) (b (make <var> #:type <int>))] (MOV a 0) (MOV b a))

* register allocation, parameters are pre-allocated registers
* method parameters, environments (variables, jumps)
* RGB, libswscale2, libmjpegtools (mjpegtools/jpegutils.h -> decode_jpeg_raw)
* nicer website
* element-wise type conversions
* getting ranges from array
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* test code examples
* scm_display(scm_target, scm_current_output_port()); printf("\n");
* reduce number of files to include
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
