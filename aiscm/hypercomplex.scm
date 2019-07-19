;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(define-module (aiscm hypercomplex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (aiscm core)
  #:use-module (aiscm util)
  #:export (make-hypercomplex jmag-part kmag-part hypercomplex
            <hypercomplex>
            <hypercomplex<>>
            <hypercomplex<float>>  <meta<hypercomplex<float>>>  <hypercomplex<float<single>>> <meta<hypercomplex<float<single>>>>
            <hypercomplex<double>> <meta<hypercomplex<double>>> <hypercomplex<float<double>>> <meta<hypercomplex<float<double>>>>)
  #:re-export (real-part imag-part equal? + - abs conj * / =))


(define-class <hypercomplex> ()
  (real #:init-keyword #:real-part #:getter real-part)
  (imag #:init-keyword #:imag-part #:getter imag-part)
  (jmag #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag #:init-keyword #:kmag-part #:getter kmag-part))

(define (make-hypercomplex a b c d)
  (make <hypercomplex> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define-method (write (self <hypercomplex>) port)
  (format port "~f~@fi~@fj~@fk" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))

(define-method (equal? (a <hypercomplex>) (b <hypercomplex>))
  (and (equal? (real-part a) (real-part b))
       (equal? (imag-part a) (imag-part b))
       (equal? (jmag-part a) (jmag-part b))
       (equal? (kmag-part a) (kmag-part b))))

(define-method (= (a <hypercomplex>) (b <hypercomplex>))
  (and (= (real-part a) (real-part b))
       (= (imag-part a) (imag-part b))
       (= (jmag-part a) (jmag-part b))
       (= (kmag-part a) (kmag-part b))))
(define-method (= (a <hypercomplex>) (b <complex>))
  (= a (make-hypercomplex (real-part b) (imag-part b) 0 0)))
(define-method (= (a <complex>) (b <hypercomplex>))
  (= (make-hypercomplex (real-part a) (imag-part a) 0 0) b))

(define-structure hypercomplex make-hypercomplex (real-part imag-part jmag-part kmag-part))
(define-uniform-constructor hypercomplex)

(define <hypercomplex<float>>  (hypercomplex <float> )) (define <meta<hypercomplex<float>>>  (class-of (hypercomplex <float> )))
(define <hypercomplex<double>> (hypercomplex <double>)) (define <meta<hypercomplex<double>>> (class-of (hypercomplex <double>)))

(define-method (native-type (value <hypercomplex>) . args)
  (if (every (lambda (x) (or (is-a? x <hypercomplex>) (complex? x))) args)
      (hypercomplex <double>) (next-method)))

(define-array-op jmag-part 1 channel-type jmag-part)
(define-array-op kmag-part 1 channel-type kmag-part)

(define-method (jmag-part (value <complex>)) 0.0)
(define-method (kmag-part (value <complex>)) 0.0)
(define-method (jmag-part (value <scalar>)) (typed-constant (class-of value) 0))
(define-method (kmag-part (value <scalar>)) (typed-constant (class-of value) 0))
(define-method (jmag-part (value <complex<>>)) (typed-constant (channel-type (class-of value)) 0))
(define-method (kmag-part (value <complex<>>)) (typed-constant (channel-type (class-of value)) 0))

(define-method (coerce (a <meta<hypercomplex<>>>) (b <meta<hypercomplex<>>>))
  (hypercomplex (reduce coerce #f (append (base a) (base b)))))
(define-method (coerce (a <meta<hypercomplex<>>>) (b <meta<complex<>>>))
  (hypercomplex (reduce coerce #f (append (base a) (base b)))))
(define-method (coerce (a <meta<complex<>>>) (b <meta<hypercomplex<>>>))
  (hypercomplex (reduce coerce #f (append (base a) (base b)))))
(define-method (coerce (a <meta<hypercomplex<>>>) (b <meta<scalar>>))
  (hypercomplex (reduce coerce #f (cons b (base a)))))
(define-method (coerce (a <meta<scalar>>) (b <meta<hypercomplex<>>>))
  (hypercomplex (reduce coerce #f (cons a (base b)))))

(define-method (- (a <hypercomplex<>>))
  (hypercomplex (- (real-part a)) (- (imag-part a)) (- (jmag-part a)) (- (kmag-part a))))

(define-method (abs (a <hypercomplex<>>))
  (sqrt (+ (* (real-part a) (real-part a))
           (* (imag-part a) (imag-part a))
           (* (jmag-part a) (jmag-part a))
           (* (kmag-part a) (kmag-part a)))))

(define-method (conj (a <hypercomplex<>>))
  (hypercomplex (real-part a) (- (imag-part a)) (- (jmag-part a)) (kmag-part a)))

(define-syntax-rule (define-hypercomplex-binary-op mapping reduction)
  (begin
    (define-method (mapping (a <hypercomplex<>>) (b <hypercomplex<>>))
      (reduction (mapping (real-part a) (real-part b))
                 (mapping (imag-part a) (imag-part b))
                 (mapping (jmag-part a) (jmag-part b))
                 (mapping (kmag-part a) (kmag-part b))))
    (define-method (mapping (a <hypercomplex<>>) (b <complex<>>))
      (reduction (mapping (real-part a) (real-part b)) (mapping (imag-part a) (imag-part b)) (jmag-part a) (kmag-part a)))
    (define-method (mapping (a <complex<>>) (b <hypercomplex<>>))
      (reduction (mapping (real-part a) (real-part b)) (mapping (imag-part a) (imag-part b)) (jmag-part b) (kmag-part b)))
    (define-method (mapping (a <hypercomplex<>>) (b <scalar>))
      (reduction (mapping (real-part a) b) (imag-part a) (jmag-part a) (kmag-part a)))
    (define-method (mapping (a <scalar>) (b <hypercomplex<>>))
      (reduction (mapping a (real-part b)) (imag-part b) (jmag-part b) (kmag-part b)))))

(define-hypercomplex-binary-op + hypercomplex)
(define-hypercomplex-binary-op - hypercomplex)

(define-method (* (a <hypercomplex<>>) (b <hypercomplex<>>))
  (hypercomplex (+ (- (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b))
                      (* (jmag-part a) (jmag-part b))) (* (kmag-part a) (kmag-part b)))
                (- (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))
                   (* (jmag-part a) (kmag-part b)) (* (kmag-part a) (jmag-part b)))
                (- (+ (* (real-part a) (jmag-part b)) (* (jmag-part a) (real-part b)))
                   (* (imag-part a) (kmag-part b)) (* (kmag-part a) (imag-part b)))
                (+ (* (real-part a) (kmag-part b)) (* (imag-part a) (jmag-part b))
                   (* (jmag-part a) (imag-part b)) (* (kmag-part a) (real-part b)))))

(define-method (* (a <hypercomplex<>>) (b <complex<>>))
  (hypercomplex (- (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))
                (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))
                (- (* (jmag-part a) (real-part b)) (* (kmag-part a) (imag-part b)))
                (+ (* (jmag-part a) (imag-part b)) (* (kmag-part a) (real-part b)))))
(define-method (* (a <complex<>>) (b <hypercomplex<>>))
  (hypercomplex (- (* (real-part a) (real-part b)) (* (imag-part a) (imag-part b)))
                (+ (* (real-part a) (imag-part b)) (* (imag-part a) (real-part b)))
                (- (* (real-part a) (jmag-part b)) (* (imag-part a) (kmag-part b)))
                (+ (* (real-part a) (kmag-part b)) (* (imag-part a) (jmag-part b)))))
(define-method (* (a <hypercomplex<>>) (b <scalar>))
  (hypercomplex (* (real-part a) b) (* (imag-part a) b) (* (jmag-part a) b) (* (kmag-part a) b)))
(define-method (* (a <scalar>) (b <hypercomplex<>>))
  (hypercomplex (* a (real-part b)) (* a (imag-part b)) (* a (jmag-part b)) (* a (kmag-part b))))

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
(define (complex-inverse a)
  (jit-let [(denom (+ (* (real-part a) (real-part a)) (* (imag-part a) (imag-part a))))]
    (complex (/ (real-part a) denom) (/ (- (imag-part a)) denom))))

(define-method (/ (a <hypercomplex<>>) (b <hypercomplex<>>))
  (jit-let [(inverse (hypercomplex-inverse b))] (* a inverse)))
(define-method (/ (a <hypercomplex<>>) (b <complex<>>))
  (jit-let [(inverse (complex-inverse b))] (* a inverse)))
(define-method (/ (a <complex<>>) (b <hypercomplex<>>))
  (jit-let [(inverse (hypercomplex-inverse b))] (* a inverse)))
(define-method (/ (a <scalar>) (b <hypercomplex<>>))
  (jit-let [(inverse (hypercomplex-inverse b))] (* a inverse)))
(define-method (/ (a <hypercomplex<>>) (b <scalar>))
  (hypercomplex (/ (real-part a) b) (/ (imag-part a) b) (/ (jmag-part a) b) (/ (kmag-part a) b)))
