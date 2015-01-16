# TODO

(use-modules (oop goops) (ice-9 rdelim) (aiscm v4l2) (aiscm util) (aiscm image) (aiscm xorg))
(define (select formats)
  (for-each (lambda (i mode) (format #t "~a: ~a~&" i mode))
            (iota (length formats))
            formats)
  (format #t "> ")
  (list-ref formats (string->number (read-line (current-input-port)))))
(define v (make <v4l2> #:device "/dev/video1" #:select select))
(show (lambda () (grab v)))
(destroy v)

(use-modules (aiscm jit) (aiscm int) (oop goops) (srfi srfi-1) (srfi srfi-26))
(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))
(define prog (list (JE 'x) (MOV a 0) 'x (MOV b a) (RET)))
(live prog)

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
