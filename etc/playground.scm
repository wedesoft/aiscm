(use-modules (oop goops) (aiscm llvm) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define-class <quaternion> ()
              (real #:init-keyword #:real #:getter real-part)
              (imag #:init-keyword #:imag #:getter imag-part)
              (jmag #:init-keyword #:jmag #:getter jmag-part)
              (kmag #:init-keyword #:kmag #:getter kmag-part))
(define (make-quaternion a b c d) (make <quaternion> #:real a #:imag b #:jmag c #:kmag d))
(define-structure quaternion make-quaternion (real-part imag-part jmag-part kmag-part))

;(define-method (- (value <quaternion<>>))
;  (quaternion (- (real-part value))
;              (- (imag-part value))
;              (- (jmag-part value))
;              (- (kmag-part value))))

(define-method (quaternion value-a value-b value-c value-d)
  (let* [(target  (reduce coerce #f (map class-of (list value-a value-b value-c value-d))))
         (adapt-a (to-type target value-a))
         (adapt-b (to-type target value-b))
         (adapt-c (to-type target value-c))
         (adapt-d (to-type target value-d))]
    (make (quaternion target)
          #:value (lambda (fun) (append ((get adapt-a) fun) ((get adapt-b) fun) ((get adapt-c) fun) ((get adapt-d) fun))))))

(define-method (store address (value <quaternion<>>))
  (llvm-begin
    (store address (real-part value))
    (store (+ address (size-of (base (class-of value)))) (imag-part value))
    (store (+ address (* 2 (size-of (base (class-of value))))) (jmag-part value))
    (store (+ address (* 3 (size-of (base (class-of value))))) (kmag-part value))))

(llvm-typed (list (quaternion <float>)) identity)
