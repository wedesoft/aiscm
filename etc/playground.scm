(use-modules (srfi srfi-1) (oop goops) (aiscm core) (aiscm util))

(define-class <hypercomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part)
  (jmag #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag #:init-keyword #:kmag-part #:getter kmag-part))

(define-method (write (self <hypercomplex>) port)
  (format port "~a+~ai+~aj+~ak" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))

(define (make-hypercomplex a b c d)
  (make <hypercomplex> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define-structure hypercomplex make-hypercomplex (real-part imag-part jmag-part kmag-part))
(define-uniform-constructor hypercomplex)

(define-array-op jmag-part 1 channel-type jmag-part)
(define-array-op kmag-part 1 channel-type kmag-part)

(define-method (native-type (value <hypercomplex>) . args)
  (if (every (lambda (x) (or (is-a? x <hypercomplex>) (complex? x))) args)
      (hypercomplex <double>) (next-method)))

(define-method (jmag-part (value <complex>)) 0.0)
(define-method (kmag-part (value <complex>)) 0.0)
(define-method (jmag-part (value <complex<>>)) (typed-constant (channel-type (class-of value)) 0))
(define-method (kmag-part (value <complex<>>)) (typed-constant (channel-type (class-of value)) 0))
(define-method (jmag-part (value <scalar>)) (typed-constant (class-of value) 0))
(define-method (kmag-part (value <scalar>)) (typed-constant (class-of value) 0))

(define-method (coerce (a <meta<hypercomplex<>>>) (b <meta<hypercomplex<>>>))
  (hypercomplex (coerce (reduce coerce #f (base a)) (reduce coerce #f (base b)))))
;
(define-method (+ (value-a <hypercomplex<>>) (value-b <hypercomplex<>>))
  (hypercomplex (+ (real-part value-a) (real-part value-b))
                (+ (imag-part value-a) (imag-part value-b))
                (+ (jmag-part value-a) (jmag-part value-b))
                (+ (kmag-part value-a) (kmag-part value-b))))

(define x (to-array (list (make-hypercomplex 1 2 3 4) 5+6i 7)))
