(use-modules (srfi srfi-1) (oop goops) (aiscm core) (aiscm util) (aiscm hypercomplex))

(define (hypercomplex-inverse a)
  (jit-let [(d1 (- (real-part a) (kmag-part a)))
            (d2 (+ (imag-part a) (jmag-part a)))
            (d3 (+ (real-part a) (kmag-part a)))
            (d4 (- (imag-part a) (jmag-part a)))
            (denom (* (+ (* d1 d1) (* d2 d2)) (+ (* d3 d3) (* d4 d4))))
            (squares (+ (* (real-part a) (real-part a))
                        (* (imag-part a) (imag-part a))
                        (* (jmag-part a) (jmag-part a))
                        (* (kmag-part a) (kmag-part a))))
            (cross (- (* (real-part a) (kmag-part a)) (* (imag-part a) (jmag-part a))))
            (c1 (* (kmag-part a) cross))
            (c2 (* (jmag-part a) cross))
            (c3 (* (imag-part a) cross))
            (c4 (* (real-part a) cross))]
    (hypercomplex (/ (- (* (real-part a) squares) (+ c1 c1)) denom)
                  (/ (- (- (* (imag-part a) squares)) (+ c2 c2)) denom)
                  (/ (- (- (* (jmag-part a) squares)) (+ c3 c3)) denom)
                  (/ (- (* (kmag-part a) squares) (+ c4 c4)) denom))))

(define-method (/ (a <hypercomplex<>>) (b <hypercomplex<>>))
  (jit-let [(inv (hypercomplex-inverse b))]
    (* a inv)))


((jit (list (hypercomplex <double>) (hypercomplex <double>)) /) (make-hypercomplex 2 3 0 0) (make-hypercomplex 2 3 0 0))
