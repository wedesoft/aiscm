# TODO

* live times, register allocation, parameters are pre-allocated registers
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
* ./configure CC=colorgcc
