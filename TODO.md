# TODO

* bug: prevent register arguments from being overwritten
  (take '(1 2 3 4) 2)
  (fold-right delete '(1 2 3 4 5) '(2 3))
* Jcc rel32 (tests)
* test 3D operation
* rename *p -> *r
* simple and documented way to dump binary to file and disassemble
* extra representation for stack values and arguments?
* (lambda (fun ...) (env fun [(r (reg ... fun)) ...] ...)) ->
  (env (r_ a_) [(r (reg ...) ...] ...))
* PPM I/O
* print arrays
* getting ranges from array
