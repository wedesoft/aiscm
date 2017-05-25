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
(use-modules (srfi srfi-26)
             (srfi srfi-64)
             (oop goops)
             (aiscm composite)
             (aiscm complex)
             (aiscm element)
             (aiscm int)
             (aiscm obj)
             (aiscm float)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm jit)
             (aiscm expression)
             (aiscm tensor))


(test-begin "aiscm complex")

(define ctx (make <context>))
(define v (make (complex <byte>) #:value 2+3i))
(define r (make (complex <byte>) #:value 5))
(test-eq "equality of complex types"
  (complex <int>) (complex <int>))
(test-eqv "storage size of byte complex"
  2 (size-of (complex <byte>)))
(test-eqv "storage size of single-precision floating-point complex"
  8 (size-of (complex <float>)))
(test-eq "base of integer complex type"
  <int> (base (complex <int>)))
(test-equal "pack complex value"
  #vu8(#x02 #x03) (pack v))
(test-equal "pack complex value with zero imaginary component"
  #vu8(#x05 #x00) (pack r))
(test-equal "unpack complex value"
  v (unpack (complex <byte>) #vu8(#x02 #x03)))
(test-assert "complex has no dimensions"
  (null? (shape v)))
(test-eq "coerce complex and scalar type"
  (complex <byte>) (coerce (complex <byte>) <byte>))
(test-eq "coerce scalar type and complex"
  (complex <byte>) (coerce <byte> (complex <byte>)))
(test-eq "coerce different complex types"
  (complex <int>) (coerce (complex <byte>) (complex <usint>)))
(test-eq "coerce complex array from array types"
  (sequence (complex <sint>)) (complex <sint> (sequence <ubyte>)))
(test-eq "coerce 2D complex array from array types"
  (multiarray (complex <int>) 2) (complex <sint> (multiarray <usint> 2)))
(test-eq "coerce integer sequence and complex type"
  (sequence (complex <int>)) (coerce (sequence <int>) (complex <int>)))
(test-eq "coerce complex type and integer sequence"
  (sequence (complex <int>)) (coerce (complex <int>) (sequence <int>)))
(test-eq "coerce complex type and 2D array"
  (multiarray (complex <int>) 2) (coerce (complex <int>) (multiarray <int> 2)))
(test-equal "'unbuild' extracts the components of a complex value"
  (list 2 3) (unbuild (complex <int>) 2+3i))
(test-eq "type matching for 2+3i"
  (complex <ubyte>) (native-type 2+3i))
(test-skip 2)
(test-eq "type matching for complex value and scalar"
  (complex <double>) (native-type 2+3i 1.2))
(test-eq "type matching for scalar and complex value"
  (complex <double>) (native-type 1.2 2+3i))
(test-eq "base type of sequence applies to element type"
  (sequence <int>) (base (sequence (complex <int>))))
(test-eqv "conjugate of complex number"
  2-3i (conj 2+3i))
(test-eqv "Return complex number"
  2+3i ((jit ctx (list (complex <int>)) identity) 2+3i))
(test-eqv "Extract real component in compiled code"
  2 ((jit ctx (list (complex <int>)) real-part) 2+3i))
(test-equal "Real part of complex array"
  '(2 5) (to-list (real-part (seq 2+3i 5+7i))))
(test-eqv "Extract imaginary component in compiled code"
  3 ((jit ctx (list (complex <int>)) imag-part) 2+3i))
(test-equal "Imaginary part of complex array"
  '(3 7) (to-list (imag-part (seq 2+3i 5+7i))))
(test-equal "compose complex value in compiled code"
  2+3i ((jit ctx (list <int> <int>) (lambda (re im) (complex re im))) 2 3))
(test-equal "convert byte complex to integer complex"
  2+3i ((jit ctx (list (complex <ubyte>)) (cut to-type (complex <int>) <>)) 2+3i))
(test-eqv "add complex values"
  7+10i ((jit ctx (list (complex <int>) (complex <int>)) +) 2+3i 5+7i))
(test-eqv "add complex and real value"
  6+3i ((jit ctx (list (complex <int>) <int>) +) 2+3i 4))
(test-eqv "add real and complex value"
  5+4i ((jit ctx (list <int> (complex <int>)) +) 2 3+4i))
(test-eqv "negate complex number"
  -2-3i ((jit ctx (list (complex <int>)) -) 2+3i))
(test-eqv "multiply complex numbers"
  -11+29i ((jit ctx (list (complex <int>) (complex <int>)) *) 2+3i 5+7i))
(test-eqv "multiply complex numbers and real value"
  10+15i ((jit ctx (list (complex <int>) <int>) *) 2+3i 5))
(test-eqv "multiply real number and complex number"
  6+10i ((jit ctx (list <int> (complex <int>)) *) 2 3+5i))
(test-eqv "divide complex numbers"
  5+7i ((jit ctx (list (complex <int>) (complex <int>)) /) -11+29i 2+3i))
(test-eqv "divide complex number by number"
  2+3i ((jit ctx (list (complex <int>) <int>) /) 4+6i 2))
(test-eqv "divide number by complex number"
  3-4i ((jit ctx (list <int> (complex <int>)) /) 25 3+4i))
(test-eqv "get real part of real number"
  42 ((jit ctx (list <int>) real-part) 42))
(test-equal "real part of array is array"
  '(2 3 5) (to-list (real-part (seq 2 3 5))))
(test-equal "Compile code to get imaginary part of real array"
  '(0 0 0) (to-list ((jit ctx (list (sequence <int>)) imag-part) (seq <int> 2 3 5))))
(test-equal "imaginary part of array is array of zeros"
  '(0 0 0) (to-list (imag-part (seq 2 3 5))))
(test-eqv "get imaginary part of real number"
  0 ((jit ctx (list <int>) imag-part) 42))
(test-eqv "complex conjugate"
  2-3i ((jit ctx (list (complex <int>)) conj) 2+3i))
(test-eqv "conjugate of real number"
  2 ((jit ctx (list <int>) conj) 2))
(let [(c (parameter (complex <int>)))]
  (test-assert "Decompose complex parameters into internal complex values"
    (is-a? (decompose-value (complex <int>) c) <internalcomplex>)))
(test-assert "complex integer memory is pointerless"
  (pointerless? (complex <int>)))
(test-assert "complex object memory is not pointerless"
  (not (pointerless? (complex <obj>))))
(test-eqv "extract real part of object complex"
  2 ((jit ctx (list (complex <obj>)) real-part) 2+3i))
(test-eqv "extract imaginary part of object complex"
  3 ((jit ctx (list (complex <obj>)) imag-part) 2+3i))
(test-equal "extract real part of complex object array"
  (list 2.0) (to-list (real-part (seq (complex <obj>) 2+3i))))
(test-equal "extract imaginary part of complex object array"
  (list 3.0) (to-list (imag-part (seq (complex <obj>) 2+3i))))
(test-equal "components of complex values are real-part and imag-part"
  (list real-part imag-part) (components <complex<>>))

(test-begin "cumulative tensor operations")
  (test-equal "Tensor sum of complex values"
    5+8i (tensor (sum i (get (seq 2+3i 3+5i) i))))
  (test-equal "Tensor product of complex values"
    -9+19i (tensor (prod i (get (seq (complex <int>) 2+3i 3+5i) i))))
(test-end "cumulative tensor operations")
(test-end "aiscm complex")
