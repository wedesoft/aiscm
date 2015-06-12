(use-modules (aiscm sequence)
             (aiscm int)
             (aiscm op)
             (oop goops))

(load-extension "libguile-bench" "init_bench")

(define-syntax-rule (run description n body ...)
  (let [(t0 (times))]
    (do ((i 0 (1+ i))) ((>= i n)) body ...)
    (let* [(t1      (times))
           (user    (- (tms:utime t1) (tms:utime t0)))
           (system  (- (tms:stime t1) (tms:stime t0)))
           (clock   (- (tms:clock t1) (tms:clock t0)))]
      (format #t "~32a ~10,6f ~10,6f ~10,6f (~10,6f)~&" description
        (/ (* 1.0e-9 user) n)
        (/ (* 1.0e-9 system) n)
        (/ (* 1.0e-9 (+ user system)) n)
        (/ (* 1.0e-9 clock) n)))))

(format #t "~32t ~10@a ~10@a ~10@a  ~10@a~&" "user" "system" "total" "real")
(run "Guile make sequence" 100 (make (sequence <int>) #:size 1000000))
(run "C allocate memory" 100 (allocation 1000000))
