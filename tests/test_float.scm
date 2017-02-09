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
             (aiscm element)
             (aiscm float)
             (aiscm jit))


(test-begin "aiscm float")

(test-equal "equality of classes"
  (floating-point single-precision) (floating-point single-precision))
(test-eq "determine precision of single-precision floating point class"
  single-precision (precision (floating-point single-precision)))
(test-eq "determine precision of double-precision floating point class"
  double-precision (precision (floating-point double-precision)))
(test-assert "check whether single-precision floating point is double"
  (not (double? (floating-point single-precision))))
(test-assert "check whether double-precision floating point is double"
  (double? (floating-point double-precision)))
(test-equal "equality of predefined clases"
  <float> (floating-point single-precision))
(test-eqv "size of single-precision floating point number"
  4 (size-of <float>))
(test-eqv "size of double-precision floating point number"
  8 (size-of <double>))
(test-equal "pack single-precision floating point number"
  #vu8(#x00 #x00 #xc0 #x3f) (pack (make <float> #:value 1.5)))
(test-equal "pack double-precision floating point number"
  #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40) (pack (make <double> #:value 3.14)))
(test-equal "display floating point object"
  "#<<float<single>> 3.14>" (call-with-output-string (lambda (port) (display (make <float> #:value 3.14) port))))
(test-equal "unpack single-precision floating point number"
  (make <float> #:value 1.5) (unpack <float> #vu8(#x00 #x00 #xc0 #x3f)))
(test-equal "unpack double-precision floating point number"
  (make <double> #:value 3.14) (unpack <double> #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)))
(test-equal "coercion of single-precision types"
  <float> (coerce <float> <float>))
(test-equal "coercion of double-precision types"
  <double> (coerce <double> <double>))
(test-equal "coercion of single- and double-precision types"
  <double> (coerce <float> <double>))
(test-equal "foreign type of single-precision floating point number"
  float (foreign-type <float>))
(test-equal "foreign type of double-precision floating point number"
  double (foreign-type <double>))
(test-skip 1)
(test-equal "type matching for 1.5"
  <double> (native-type 1.5))
(test-eqv "get value of floating point number"
  1.25 (get (make <float> #:value 1.25)))
(test-eqv "set value of floating point number"
  1.25 (let [(i (make <float> #:value 0))] (set i 1.25) (get i)))
(test-eqv "return-value of setting floating point number"
  1.25 (set (make <float> #:value 0) 1.25))
(test-equal "build floating point number"
  1.25 (build <float> (list 1.25)))
(test-equal "'content' returns floating point values"
  '(1.25) (content <float> 1.25))
(test-assert "floating-point memory is pointerless"
  (pointerless? <float>))

(test-end "aiscm float")
