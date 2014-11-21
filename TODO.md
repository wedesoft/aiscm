# TODO

* register allocator, RGB, swscale
* nicer website
* element-wise type conversions
* getting ranges from array
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* test code examples
* scm_display(scm_target, scm_current_output_port()); printf("\n");
* reduce number of files to include
