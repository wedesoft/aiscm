# TODO

* sorting of V4L2 format selection
* compute proper frame size
* close windows when closing display
* register allocator, RGB, swscale
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* print arrays
* getting ranges from array
