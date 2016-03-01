(use-modules (oop goops)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm jit)
             (aiscm op)
             (aiscm util))

(load-extension "libguile-bench" "init_bench")

(define hook #t); prevent optimizer from removing benchmarked code

(define-syntax-rule (run description n body ...)
  (begin
    body ...
    (let [(t0 (times))]
      (do ((i 0 (1+ i))) ((>= i n)) (set! hook (begin body ...)))
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
(define ptr (gc-malloc-pointerless (* (size-of <int>) size)))
(define <sequence<int>> (sequence <int>))
(define empty (make <sequence<int>> #:size 0))
(define s (make <sequence<int>> #:size size))
(define-class <c> ())
(define-method (neg (self <c>)) self)
(define c (make <c>))

(format #t "~32t ~10@a ~10@a ~10@a  ~10@a~&" "user" "system" "total" "real")

(run "Guile GOOPS method dispatch" n (neg c))
(run "Guile make empty sequence" n (make <sequence<int>> #:size 0))
(run "Guile allocate memory" n (gc-malloc-pointerless (* (size-of <int>) size)))
(run "Guile negate empty sequence" n (- empty))
(run "Guile make sequence" n (make <sequence<int>> #:size size))
(run "Guile negate sequence" n (- s))
(run "C allocate memory" n (allocation (* (size-of <int>) size)))
(run "C negate empty sequence" n (negate ptr 1 0))
(run "C negate sequence" n (negate ptr 1 size))
