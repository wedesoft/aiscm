# TODO

(use-modules (oop goops) (ice-9 rdelim) (aiscm v4l2) (aiscm util) (aiscm image) (aiscm xorg) (srfi srfi-1) (srfi srfi-26))
(define v (make <v4l2> #:device "/dev/video1" #:select last))
(show (lambda () (grab v)))
(destroy v)

(define d (make <xdisplay> #:name ":0.0"))
(define w (make <xwindow> #:display d #:shape '(1280 720) #:io IO-XVIDEO))
(title= w "Test")
(show w)
(do () ((quit? d)) (write w (grab v)) (process-events d))
(quit= d #f)
(hide w)

(define (select formats)
  (for-each (lambda (i mode) (format #t "~a: ~a~&" i mode))
            (iota (length formats))
            formats)
  (format #t "> ")
  (list-ref formats (string->number (read-line (current-input-port)))))
(define v (make <v4l2> #:device "/dev/video1" #:select select))


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
* use ffmpeg library to convert MJPEG -> YV12, UYVY, ...
  http://www.codeproject.com/Questions/744389/Trying-to-setup-MJPEG-encoder-in-ffmpeg-in-Cpluspl
