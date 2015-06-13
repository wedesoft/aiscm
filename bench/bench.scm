(use-modules (oop goops)
             (rnrs bytevectors)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm op)
             (aiscm util))

(load-extension "libguile-bench" "init_bench")

(define-syntax-rule (run description n body ...)
  (begin
    body ...
    (let [(t0 (times))]
      (do ((i 0 (1+ i))) ((>= i n)) body ...)
      (gc)
      (let* [(t1      (times))
             (user    (- (tms:utime t1) (tms:utime t0)))
             (system  (- (tms:stime t1) (tms:stime t0)))
             (clock   (- (tms:clock t1) (tms:clock t0)))]
        (format #t "~32a ~10,6f ~10,6f ~10,6f (~10,6f)~&" description
          (/ (* 1.0e-9 user) n)
          (/ (* 1.0e-9 system) n)
          (/ (* 1.0e-9 (+ user system)) n)
          (/ (* 1.0e-9 clock) n))))))

(define n 1000)
(define size 250000)
(define ptr (malloc (* (size-of <int>) n)))
(define s (make (sequence <int>) #:size size))
(- s)

(format #t "~32t ~10@a ~10@a ~10@a  ~10@a~&" "user" "system" "total" "real")

(run "Guile make byte vector" n (make-bytevector (* (size-of <int>) size)))
(run "Guile make sequence" n (make (sequence <int>) #:size size))
(run "C allocate memory" n (allocation size))
(run "Guile negate sequence" n (- s))
(run "C negate sequence" n (negate ptr size))
