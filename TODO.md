# TODO

* V4L2
* implement infinite register machine (register allocator)
  nur argumente evaluieren (e.g. (MOV rl (get-value r_)) -> (MOV (reg:LI 2) (arg 5)))
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* print arrays
* getting ranges from array
* use bytevectors? http://www.gnu.org/software/guile/manual/html_node/Dynamic-FFI.html
