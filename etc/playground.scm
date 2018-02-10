(use-modules (oop goops) (aiscm llvm) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define-class <quaternion> ()
              (real #:init-keyword #:real #:getter real-part)
              (imag #:init-keyword #:imag #:getter imag-part)
              (jmag #:init-keyword #:jmag #:getter jmag-part)
              (kmag #:init-keyword #:kmag #:getter kmag-part))
(define (make-quaternion a b c d) (make <quaternion> #:real a #:imag b #:jmag c #:kmag d))
(define-structure quaternion make-quaternion (real-part imag-part jmag-part kmag-part))

(define-method (- (value <quaternion<>>))
  (quaternion (- (real-part value))
              (- (imag-part value))
              (- (jmag-part value))
              (- (kmag-part value))))

(llvm-typed (list (quaternion <float>)) -)
