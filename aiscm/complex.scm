(define-module (aiscm complex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm asm)
  #:use-module (aiscm jit)
  #:use-module (aiscm util)
  #:export (complex
            <internalcomplex>
            <complex<>> <meta<complex<>>>
            <pointer<complex<>>> <meta<pointer<complex<>>>>)
  #:re-export (real-part imag-part))

(define ctx (make <context>))

(define-method (conj self) self)
(define-method (conj (self <complex>)) (make-rectangular (real-part self) (- (imag-part self))))
(define-class <internalcomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part))
(define-method (complex re im) (make <internalcomplex> #:real-part re #:imag-part im))
(define-method (write (self <internalcomplex>) port)
  (apply format port "(complex ~a ~a)" (content self)))
(define-class* <complex<>> <element> <meta<complex<>>> <meta<element>>)
(define-method (complex (t <meta<element>>))
  (template-class (complex t) <complex<>>
    (lambda (class metaclass)
      (define-method (base (self metaclass))t)
      (define-method (size-of (self metaclass)) (* 2 (size-of t))))))
(define-method (complex (re <meta<element>>) (im <meta<element>>)) (complex (coerce re im)))
(define-method (real-part (self <int<>>)) self); TODO: use a number type
(define-method (imag-part (self <int<>>)) 0)
(define-method (real-part (self <complex<>>)) (make (base (class-of self)) #:value (real-part (get self))))
(define-method (imag-part (self <complex<>>)) (make (base (class-of self)) #:value (imag-part (get self))))
(define-method (pack (self <complex<>>)) (bytevector-concat (map pack (content self))))
(define-method (unpack (self <meta<complex<>>>) (packed <bytevector>))
  (let* [(size    (size-of (base self)))
         (vectors (map (cut bytevector-sub packed <> size) (map (cut * size <>) (iota 2))))]
    (make self #:value (apply make-rectangular (map (lambda (vec) (get (unpack (base self) vec))) vectors)))))
(define-method (content (self <complex>)) (map inexact->exact (list (real-part self) (imag-part self))))
(define-method (content (self <internalcomplex>)) (map (cut <> self) (list real-part imag-part)))
(define-method (content (self <complex<>>)) (map (cut <> self) (list real-part imag-part))); TODO: test, refactor
(define-method (coerce (a <meta<complex<>>>) (b <meta<element>>)) (complex (coerce (base a) b)))
(define-method (coerce (a <meta<element>>) (b <meta<complex<>>>)) (complex (coerce a (base b))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<complex<>>>)) (complex (coerce (base a) (base b))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<sequence<>>>)) (multiarray (coerce a (typecode b)) (dimensions b)))
(define-method (match (c <complex>) . args)
  (complex (apply match (concatenate (map-if (cut is-a? <> <complex>) content list (cons c args))))))
(define-method (build (self <meta<complex<>>>) value) (fetch value))
(define-method (base (self <meta<sequence<>>>)) (multiarray (base (typecode self)) (dimensions self)))
(define-syntax-rule (unary-complex-op op)
  (define-method (op (a <internalcomplex>)) (apply complex (map op (content a)))))
(unary-complex-op -)
(define-syntax-rule (binary-complex-op op)
  (begin
    (define-method (op (a <internalcomplex>) b) (complex (op (real-part a) b) (imag-part a)))
    (define-method (op a (b <internalcomplex>)) (complex (op a (real-part b)) (imag-part b)))
    (define-method (op (a <internalcomplex>) (b <internalcomplex>)) (apply complex (map op (content a) (content b))))))
(define-method (conj (self <int<>>)) self)
(define-method (conj (self <pointer<int<>>>)) self)
(define-method (conj (a <internalcomplex>)) (complex (real-part a) (- (imag-part a))))
(binary-complex-op +)
(binary-complex-op -)
(define-method (* (a <internalcomplex>) b) (apply complex (map (cut * <> b) (content a))))
(define-method (* a (b <internalcomplex>)) (apply complex (map (cut * a <>) (content b))))
(define-method (* (a <internalcomplex>) (b <internalcomplex>))
  (complex (- (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))
           (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))))
(define-method (/ (a <internalcomplex>) b) (apply complex (map (cut / <> b) (content a))))
(define (arg2 b) (apply + (map * (content b) (content b))))
(define-method (/ a (b <internalcomplex>))
  (let [(denom (arg2 b))]
    (complex (/ (* a (real-part b)) denom) (- (/ (* a (imag-part b)) denom)))))
(define-method (/ (a <internalcomplex>) (b <internalcomplex>))
  (let [(denom (arg2 b))]
    (complex (/ (+ (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b))) denom)
             (/ (- (* (imag-part a) (real-part b)) (* (real-part a) (imag-part b))) denom))))

(define-method (copy-value (typecode <meta<complex<>>>) a b)
  (append-map (lambda (channel) (code (channel a) (channel b))) (list real-part imag-part)))

(define-method (component (type <meta<complex<>>>) self offset)
  (let* [(type (base (typecode self)))]
    (set-pointer-offset (pointer-cast type self) (* offset (size-of type)))))
(pointer <complex<>>)
(define-method (real-part (self <pointer<int<>>>)) self)
(define-method (imag-part (self <pointer<int<>>>)) 0)
(define-method (real-part (self <pointer<complex<>>>)) (component (typecode self) self 0))
(define-method (imag-part (self <pointer<complex<>>>)) (component (typecode self) self 1))

(define-method (var (self <meta<complex<>>>)) (let [(type (base self))] (complex (var type) (var type)))); TODO: test

(define-nary-op n-ary-fun base real-part 1 unary-extract real-part)
(define-nary-op n-ary-fun base imag-part 1 unary-extract imag-part)

(n-ary-fun complex 2 complex)

(define-method (decompose-value (target <meta<complex<>>>) x)
  (make <internalcomplex> #:real-part (parameter (real-part (delegate x)))
                          #:imag-part (parameter (imag-part (delegate x)))))

(define-method (to-type (target <meta<complex<>>>) (self <internalcomplex>))
  (apply complex (map (cut to-type (base target) <>) (content self))))

(define-nary-op n-ary-fun identity conj 1 unary-extract conj)
