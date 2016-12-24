;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
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
(use-modules (oop goops)
             (system foreign)
             (aiscm element)
             (aiscm float)
             (aiscm jit)
             (guile-tap))
(ok (equal? (floating-point single-precision) (floating-point single-precision))
    "equality of classes")
(ok (eq? single-precision (precision (floating-point single-precision)))
    "determine precision of single-precision floating point class")
(ok (eq? double-precision (precision (floating-point double-precision)))
    "determine precision of double-precision floating point class")
(ok (not (double? (floating-point single-precision)))
    "check whether single-precision floating point is double")
(ok (double? (floating-point double-precision))
    "check whether double-precision floating point is double")
(ok (equal? <float> (floating-point single-precision))
    "equality of predefined clases")
(ok (eqv? 4 (size-of <float>))
    "size of single-precision floating point number")
(ok (eqv? 8 (size-of <double>))
    "size of double-precision floating point number")
(ok (equal? #vu8(#x00 #x00 #xc0 #x3f) (pack (make <float> #:value 1.5)))
    "pack single-precision floating point number")
(ok (equal? #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
            (pack (make <double> #:value 3.14)))
    "pack double-precision floating point number")
(ok (equal? "#<<float<single>> 3.14>"
            (call-with-output-string (lambda (port) (display (make <float> #:value 3.14) port))))
    "display floating point object")
(ok (equal? (make <float> #:value 1.5) (unpack <float> #vu8(#x00 #x00 #xc0 #x3f)))
    "unpack single-precision floating point number")
(ok (equal? (make <double> #:value 3.14) (unpack <double> #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)))
    "unpack double-precision floating point number")
(ok (equal? <float> (coerce <float> <float>))
    "coercion of single-precision types")
(ok (equal? <double> (coerce <double> <double>))
    "coercion of double-precision types")
(ok (equal? <double> (coerce <float> <double>))
    "coercion of single- and double-precision types")
(ok (equal? float (foreign-type <float>))
    "foreign type of single-precision floating point number")
(ok (equal? double (foreign-type <double>))
    "foreign type of double-precision floating point number")
(skip (equal? <double> (native-type 1.5))
    "type matching for 1.5")
(ok (eqv? 1.25 (get (make <float> #:value 1.25)))
    "get value of floating point number")
(ok (eqv? 1.25 (let [(i (make <float> #:value 0))] (set i 1.25) (get i)))
    "set value of floating point number")
(ok (eqv? 1.25 (set (make <float> #:value 0) 1.25))
    "return-value of setting floating point number")
(ok (equal? 1.25 (build <float> (list 1.25)))
    "build floating point number")
(ok (equal? '(1.25) (content <float> 1.25))
    "'content' returns floating point values")
(ok (pointerless? <float>)
    "floating-point memory is pointerless")
(run-tests)
