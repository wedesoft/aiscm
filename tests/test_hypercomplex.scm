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
(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (oop goops)
             (system foreign)
             (rnrs bytevectors)
             (aiscm core)
             (aiscm hypercomplex))


(test-begin "aiscm hypercomplex")

(test-group "hypercomplex values"
  (test-eqv "real part"
    2 (real-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "imag part"
    3 (imag-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "jmag part"
    5 (jmag-part (make-hypercomplex 2 3 5 7)))
  (test-eqv "kmag part"
    7 (kmag-part (make-hypercomplex 2 3 5 7)))
  (test-equal "hypercomplex equality"
    (make-hypercomplex 2 3 5 7) (make-hypercomplex 2 3 5 7))
  (test-assert "unequal hypercomplex numbers"
    (not (equal? (make-hypercomplex 2 3 6 7) (make-hypercomplex 2 3 5 7)))))

(test-group "display or write string"
  (test-equal "write hypercomplex number"
    "1.0+2.0i+3.0j+4.0k" (call-with-output-string (cut write (make-hypercomplex 1 2 3 4) <>)))
  (test-equal "write hypercomplex number with negative components"
    "-1.0-2.0i-3.0j-4.0k" (call-with-output-string (cut display (make-hypercomplex -1 -2 -3 -4) <>))))

(test-group "hypercomplex native type"
  (test-eq "hypercomplex single precision"
    <hypercomplex<float>> (hypercomplex <float>))
  (test-eq "hypercomplex double precision"
    <hypercomplex<double>> (hypercomplex <double>))
  (test-equal "construct hypercomplex number in compiled code"
    (make-hypercomplex 2 3 5 7) ((jit (list <int> <int> <int> <int>) hypercomplex) 2 3 5 7)))

(test-group "determine native type"
  (test-eq "match hypercomplex type"
    (hypercomplex <double>) (native-type (make-hypercomplex 2 3 5 7)))
  (test-eq "match hypercomplex and complex number"
    (hypercomplex <double>) (native-type (make-hypercomplex 2 3 5 7) 2+3i))
  (test-eq "match hypercomplex number and symbol"
    <obj> (native-type (make-hypercomplex 2 3 5 7) 'a))
  (test-eq "match two hypercomplex numbers"
    (hypercomplex <double>) (native-type (make-hypercomplex 2 3 5 7) (make-hypercomplex 3 5 7 11))))

(test-group "extract channels from hypercomplex array"
  (test-equal "extract real part"
    '(2.0) (to-list (real-part (to-array (list (make-hypercomplex 2 3 5 7))))))
  (test-equal "extract imaginary part"
    '(3.0) (to-list (imag-part (to-array (list (make-hypercomplex 2 3 5 7))))))
  (test-equal "extract jmaginary part"
    '(5.0) (to-list (jmag-part (to-array (list (make-hypercomplex 2 3 5 7))))))
  (test-equal "extract kmaginary part"
    '(7.0) (to-list (kmag-part (to-array (list (make-hypercomplex 2 3 5 7))))))
  (test-equal "jmaginary part of complex number"
    0.0 (jmag-part 2+3i))
  (test-equal "jmaginary part of complex array"
    '(0.0) (to-list (jmag-part (to-array (list 2+3i)))))
  (test-equal "kmaginary part of complex number"
    0.0 (kmag-part 2+3i))
  (test-equal "kmaginary part of complex array"
    '(0.0) (to-list (kmag-part (to-array (list 2+3i)))))
  (test-equal "jmaginary part of real number"
    0.0 (jmag-part 3.0))
  (test-equal "jmaginary part of real array"
    '(0.0) (to-list (jmag-part (arr 3.0))))
  (test-equal "kmaginary part of real number"
    0.0 (kmag-part 3.0))
  (test-equal "kmaginary part of real array"
    '(0.0) (to-list (kmag-part (arr 3.0)))))

(test-group "type coercions"
  (test-eq "coerce double- and single-precision hypercomplex types"
    (hypercomplex <double>) (coerce (hypercomplex <double>) (hypercomplex <float>)))
  (test-eq "coerce single- and double-precision hypercomplex types"
    (hypercomplex <double>) (coerce (hypercomplex <float>) (hypercomplex <double>)))
  (test-eq "coerce hypercomplex and complex number"
    (hypercomplex <double>) (coerce (hypercomplex <float>) (complex <double>)))
  (test-eq "coerce complex and hypercomplex number"
    (hypercomplex <double>) (coerce (complex <double>) (hypercomplex <float>)))
  (test-eq "coerce hypercomplex and real type"
    (hypercomplex <double>) (coerce (hypercomplex <float>) <double>))
  (test-eq "coerce real and hypercomplex type"
    (hypercomplex <double>) (coerce <double> (hypercomplex <float>))))

(test-group "hypercomplex plus"
  (test-equal "hypercomplex plus hypercomplex"
    (make-hypercomplex 2.0 3.0 5.0 7.0) ((jit (list (hypercomplex <float>) (hypercomplex <float>)) +)
                                         (make-hypercomplex 1 2 3 4) (make-hypercomplex 1 1 2 3)))
  (test-equal "hypercomplex plus complex"
    (make-hypercomplex 2.0 3.0 5.0 7.0)  ((jit (list (hypercomplex <float>) (complex <float>)) +)
                                          (make-hypercomplex 1 2 5 7) 1+i))
  (test-equal "complex plus hypercomplex"
    (make-hypercomplex 2.0 3.0 5.0 7.0) ((jit (list (complex <float>) (hypercomplex <float>)) +)
                                         1+i (make-hypercomplex 1 2 5 7)))
  (test-equal "hypercomplex plus real"
    (make-hypercomplex 2.0 3.0 5.0 7.0) ((jit (list (hypercomplex <float>) <float>) +)
                                         (make-hypercomplex 1 3 5 7) 1))
  (test-equal "real plus hypercomplex"
    (make-hypercomplex 2.0 3.0 5.0 7.0) ((jit (list <float> (hypercomplex <float>)) +)
                                         1 (make-hypercomplex 1 3 5 7))))

(test-group "hypercomplex minus"
  (test-equal "hypercomplex unary minus"
    (make-hypercomplex -2.0 -3.0 -5.0 -7.0) ((jit (list (hypercomplex <float>)) -) (make-hypercomplex 2 3 5 7)))
  (test-equal "hypercomplex minus hypercomplex"
    (make-hypercomplex 2.0 3.0 5.0 7.0) ((jit (list (hypercomplex <float>) (hypercomplex <float>)) -)
                                         (make-hypercomplex 3 5 8 9) (make-hypercomplex 1 2 3 2)))
  (test-equal "hypercomplex minus complex"
    (make-hypercomplex 2.0 3.0 5.0 7.0)  ((jit (list (hypercomplex <float>) (complex <float>)) -)
                                          (make-hypercomplex 3 5 5 7) 1+2i)))

(define o (make-hypercomplex 1.0 0.0 0.0 0.0))
(define no (make-hypercomplex -1.0 0.0 0.0 0.0))
(define i (make-hypercomplex 0.0 1.0 0.0 0.0))
(define ni (make-hypercomplex 0.0 -1.0 0.0 0.0))
(define j (make-hypercomplex 0.0 0.0 1.0 0.0))
(define nj (make-hypercomplex 0.0 0.0 -1.0 0.0))
(define k (make-hypercomplex 0.0 0.0 0.0 1.0))
(define nk (make-hypercomplex 0.0 0.0 0.0 -1.0))
(test-group "hypercomplex multiplication"
  (for-each (lambda (a results)
    (for-each (lambda (b result)
      (test-equal (format #f "~a times ~a" a b)
        result ((jit (list (hypercomplex <float>) (hypercomplex <float>)) *) a b)))
      (list o i j k) results))
    (list o i j k)
    (list (list o  i  j  k)
          (list i no  k nj)
          (list j  k no ni)
          (list k nj ni  o))))
(test-group "complex-hypercomplex multiplication"
  (for-each (lambda (a results)
    (for-each (lambda (b result)
      (test-equal (format #f "~a times ~a" a b)
        result ((jit (list (complex <float>) (hypercomplex <float>)) *) a b))
      (test-equal (format #f "~a times ~a" b a)
        result ((jit (list (hypercomplex <float>) (complex <float>)) *) b a)))
      (list o i j k) results))
    (list 1 +i)
    (list (list o  i  j  k)
          (list i no  k nj))))
(test-group "real-hypercomplex multiplication"
  (test-equal "multiply real and hypercomplex number"
    (make-hypercomplex 4.0 6.0 10.0 14.0) ((jit (list <float> (hypercomplex <float>)) *) 2 (make-hypercomplex 2 3 5 7)))
  (test-equal "multiply hypercomplex and real number"
    (make-hypercomplex 4.0 6.0 10.0 14.0) ((jit (list (hypercomplex <float>) <float>) *) (make-hypercomplex 2 3 5 7) 2)))

(test-group "hypercomplex division"
  (let [(f (jit (list (hypercomplex <float>) (hypercomplex <float>)) /))]
    (test-approximate "real part is one when dividing number by itself"
      1.0 (real-part (f (make-hypercomplex 2 3 5 7) (make-hypercomplex 2 3 5 7))) 0.001)
    (test-approximate "imaginary part is zero when dividing number by itself"
      0.0 (imag-part (f (make-hypercomplex 2 3 5 7) (make-hypercomplex 2 3 5 7))) 0.001)
    (test-approximate "jmaginary part is zero when dividing number by itself"
      0.0 (jmag-part (f (make-hypercomplex 2 3 5 7) (make-hypercomplex 2 3 5 7))) 0.001)
    (test-approximate "kmaginary part is zero when dividing number by itself"
      0.0 (kmag-part (f (make-hypercomplex 2 3 5 7) (make-hypercomplex 2 3 5 7))) 0.001))
  (let [(f (jit (list (hypercomplex <float>) (complex <float>)) /))]
    (test-approximate "real part is one for trivial hypercomplex-complex division"
      1.0 (real-part (f (make-hypercomplex 2 3 0 0) 2+3i)) 0.001))
  (let [(f (jit (list (complex <float>) (hypercomplex <float>)) /))]
    (test-approximate "real part is one for trivial complex-hypercomplex division"
      1.0 (real-part (f 2+3i (make-hypercomplex 2 3 0 0))) 0.001))
  (let [(f (jit (list <float> (hypercomplex <float>)) /))]
    (test-approximate "real part is one for trivial float-hypercomplex division"
      1.0 (real-part (f 2 (make-hypercomplex 2 0 0 0))) 0.001))
  (let [(f (jit (list (hypercomplex <float>) <float>) /))]
    (test-approximate "real part is one for trivial hypercomplex-float division"
      1.0 (real-part (f (make-hypercomplex 2 0 0 0) 2)) 0.001)))

(test-group "hypercomplex properties"
  (test-equal "hypercomplex absolute value"
    4.0 ((jit (list (hypercomplex <float>)) abs) (make-hypercomplex 2 2 -2 -2)))
  (test-equal "hypercomplex conjugate"
    (make-hypercomplex 2.0 -3.0 -5.0 7.0) ((jit (list (hypercomplex <float>)) conj) (make-hypercomplex 2 3 5 7))))

(test-end "aiscm hypercomplex")
