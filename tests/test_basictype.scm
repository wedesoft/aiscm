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
        nbits (bits (make cls #:value 0)))
      (test-eq (format #f "~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? cls))
      (test-eq (format #f "instance of ~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? (make cls #:value 0))))
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
  (test-eqv "Wrap and unwrap value"
    42 (get (make <int> #:value 42)))
  (test-equal "Equal values"
    (make <int> #:value 42) (make <int> #:value 42))
  (test-assert "Unequal values"
    (not (equal? (make <int> #:value 42) (make <int> #:value 43))))
  (test-assert "Unequal types values"
    (not (equal? (make <uint> #:value 42) (make <int> #:value 42))))
(test-end "integer values")

(test-begin "get foreign type")
  (for-each (lambda (type foreign)
    (test-eqv (format #f "get foreign type of ~a" (class-name type))
      foreign (foreign-type type)))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>)
    (list uint8   int8   uint16  int16  uint32 int32 uint64  int64))
(test-end "get foreign type")

(test-end "aiscm basictype")
