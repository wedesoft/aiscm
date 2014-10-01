# TODO

* correct pitches and offsets for Xvideo frames
* memory alignment for frames
* getting ranges from array
* register allocator, RGB, swscale
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* convert multiarray to image with strides and pitches
* documentation: (do () ((quit? d)) (write w (grab v)) (process-events d))
* scm_display(scm_target, scm_current_output_port()); printf("\n");
