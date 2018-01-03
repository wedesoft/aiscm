;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
(use-modules (srfi srfi-64)
             (oop goops)
             (system foreign)
             (aiscm basictype))


(test-begin "aiscm basictype")
(test-begin "construct integer types")
  (for-each
    (lambda (nbits sign cls)
      (test-eq (format #f "Class for ~a-bit ~a integer should be ~a" nbits sign (class-name cls))
        cls (integer nbits sign))
      (test-eqv (format #f "~a should have ~a bits" (class-name cls) nbits)
        nbits (bits cls))
      (test-eqv (format #f "instance of ~a should have ~a bits" (class-name cls) nbits)
        nbits (bits (make cls #:value #f)))
      (test-eq (format #f "~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? cls))
      (test-eq (format #f "instance of ~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? (make cls #:value #f))))
    (list 8 8 16 16 32 32 64 64)
    (list unsigned signed unsigned signed unsigned signed unsigned signed)
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
(test-end "construct integer types")

(test-begin "integer coercions")
  (for-each
    (lambda (a b result)
      (test-eq (format #f "Type coercion of ~a and ~a should return ~a" (class-name a) (class-name b) (class-name result))
        result (coerce a b)))
    (list <sint> <int>  <sint> <uint> <int>   <usint> <byte> <uint> <ulong>)
    (list <sint> <sint> <int>  <uint> <usint> <int>   <uint> <byte> <long> )
    (list <sint> <int>  <int>  <uint> <int>   <int>   <long> <long> <long> ))
(test-end "integer coercions")

(test-begin "integer values")
  (test-equal "Wrap and unwrap value"
    '(42) (get (make <int> #:value '(42))))
  (test-equal "Equal values"
    (make <int> #:value '(42)) (make <int> #:value '(42)))
  (test-assert "Unequal values"
    (not (equal? (make <int> #:value '(42)) (make <int> #:value '(43)))))
  (test-assert "Unequal types values"
    (not (equal? (make <uint> #:value '(42)) (make <int> #:value '(42)))))
(test-end "integer values")

(test-begin "get foreign integer type")
  (for-each (lambda (type foreign)
    (test-eqv (format #f "get foreign type of ~a" (class-name type))
      foreign (foreign-type type)))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>)
    (list uint8   int8   uint16  int16  uint32 int32 uint64  int64))
(test-end "get foreign integer type")

(test-begin "construct floating point types")
  (test-assert "single precision"
    (not (double-precision? (floating-point single-precision))))
  (test-assert "double precision"
    (double-precision? (floating-point double-precision)))
  (test-eq "compare single precision types"
    <float> (floating-point single-precision))
  (test-eq "compare double precision types"
    <double> (floating-point double-precision))
(test-end "construct floating point types")

(test-begin "floating-point coercions")
  (test-equal "coerce single precision floats"
    <float> (coerce <float> <float>))
  (test-equal "coerce single and double precision float"
    <double> (coerce <float> <double>))
  (test-equal "coerce double and single precision float"
    <double> (coerce <double> <float>))
  (test-equal "coerce double precision floats"
    <double> (coerce <double> <double>))
(test-end "floating-point coercions")

(test-begin "floating-point values")
  (test-equal "Wrap and unwrap value"
    '(3.5) (get (make <float> #:value '(3.5))))
  (test-equal "Equal values"
    (make <float> #:value '(3.5)) (make <float> #:value '(3.5)))
  (test-assert "Unequal values"
    (not (equal? (make <float> #:value '(3.5)) (make <float> #:value '(4.5)))))
  (test-assert "Unequal types values"
    (not (equal? (make <float> #:value '(1.5)) (make <double> #:value '(1.5)))))
(test-end "floating-point values")

(test-begin "get foreign floating-point type")
  (test-eqv "get foreign type of <float>"
    float (foreign-type <float>))
  (test-eqv "get foreign type of <double>"
    double (foreign-type <double>))
(test-end "get foreign floating-point type")

(test-begin "coercing floating-point and integer types")
  (test-eq "coerce single-precision floating point and integer"
    <float> (coerce <float> <int>))
  (test-eq "coerce double-precision floating point and integer"
    <double> (coerce <double> <int>))
  (test-eq "coerce integer and single-precision floating point"
    <float> (coerce <int> <float>))
  (test-eq "coerce integer and double-precision floating point"
    <double> (coerce <int> <double>))
(test-end "coercing floating-point and integer types")

(test-begin "complex numbers")
  (test-eq "Single-precision complex number"
    <complex<float>> (complex (floating-point single-precision)))
  (test-eq "Double-precision complex number"
    <complex<double>> (complex (floating-point double-precision)))
  (test-eq "Basic type of single-precision complex number"
    <float> (base <complex<float>>))
  (test-eq "Basic type of double-precision complex number"
    <double> (base <complex<double>>))
  (test-eq "Real component of single-precision floating-point complex number is a float"
    <float> (class-of (real-part (make <complex<float>> #:value '(2.5 3.25)))))
  (test-eq "Real component of double-precision floating-point complex number is a double"
    <double> (class-of (real-part (make <complex<double>> #:value '(2.5 3.25)))))
  (test-eqv "Get real component of complex number"
    2.5 (get (real-part (make <complex<float>> #:value '(2.5 3.25)))))
  (test-eq "Imaginary component of single-precision floating-point complex number is a float"
    <float> (class-of (imag-part (make <complex<float>> #:value '(2.5 3.25)))))
  (test-eq "Imaginary component of double-precision floating-point complex number is a double"
    <double> (class-of (imag-part (make <complex<double>> #:value '(2.5 3.25)))))
  (test-eqv "Get imaginary component of complex number"
    3.25 (get (imag-part (make <complex<float>> #:value '(2.5 3.25)))))
(test-end "complex numbers")

(test-begin "decompose arguments")
  (test-equal "Decompose integer"
    '(42) (decompose-argument <int> 42))
  (test-equal "Decompose floating-point number"
    '(1.25) (decompose-argument <float> 1.25))
  (test-equal "Decompose complex number"
     '(2.5 3.25) (decompose-argument <complex<float>> 2.5+3.25i))
(test-end "decompose arguments")

(test-begin "compose value")
  (test-eq "Composing integer creates correct type"
    <sint> (class-of (compose-value <sint> '(42))))
  (test-equal "Composing integer uses provided value"
    '(42) (get (compose-value <sint> '(42))))
  (test-equal "Compose complex number"
    '(2.5 3.25) (get (compose-value <complex<float>> '(2.5 3.25))))
(test-end "compose value")

(test-begin "compose multiple values")
  (test-assert "Compose no values"
    (null? (compose-values '() '())))
  (test-equal "Composing an integer value creates an object of correct type"
    (list <sint>) (map class-of (compose-values (list <sint>) '(42))))
  (test-equal "Composing an integer value uses the provided value"
    '(42) (get (car (compose-values (list <sint>) '(42)))))
  (test-equal "Composing a complex number uses two values"
    '(2.5 3.25) (get (car (compose-values (list <complex<float>>) '(2.5 3.25)))))
  (test-equal "Compose two integer values"
    '((5) (7)) (map get (compose-values (list <int> <int>) '(5 7))))
(test-end "compose multiple values")

(test-begin "decompose type")
  (test-equal "decompose integer type"
    (list <ubyte>) (decompose-type <ubyte>))
  (test-equal "decompose floating-point type"
    (list <float>) (decompose-type <float>))
  (test-equal "decompose complex type"
    (list <double> <double>) (decompose-type <complex<double>>))
(test-end "decompose type")

(test-begin "decompose multiple types")
  (test-equal "decompose empty list of types"
    '() (decompose-types '()))
  (test-equal "decompose integer type"
    (list <sint>) (decompose-types (list <sint>)))
  (test-equal "decompose complex type"
    (list <double> <double>) (decompose-types (list <complex<double>>)))
(test-end "decompose multiple types")
(test-end "aiscm basictype")
