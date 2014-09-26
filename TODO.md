# TODO

* use shape instead of width and height with <image>
* image <-> multiarray with strides and pitches
* X11: XVideo-output
* register allocator, RGB, swscale
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* getting ranges from array
