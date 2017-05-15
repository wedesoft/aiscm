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
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm variable)
             (aiscm expression))


(test-begin "aiscm expression")
(test-begin "skeleton of scalar expression")
(let  [(i (skeleton <int>))]
  (test-assert "skeleton of integer is of type integer"
    (is-a? i <int>))
  (test-assert "value of integer skeleton is a variable"
    (is-a? (value i) <var>))
  (test-eq "value of integer skeleton is of type integer"
    <int> (typecode (value i))))
(test-end "skeleton of scalar expression")

(test-begin "skeleton of array expression")
  (let [(s (skeleton (sequence <byte>)))]
    (test-assert "skeleton of a sequence is a sequence"
      (is-a? s (sequence <byte>)))
    (test-equal "skeleton of sequence consists of two long integer variables and an unsigned long integer"
      (list <long> <long> <ulong>) (map class-of (content (class-of s) s)))
    (test-equal "sequence skeleton is based on three variables"
      (list <var> <var> <var>) (map class-of (map get (content (class-of s) s)))))
  (let [(m (skeleton (multiarray <int> 2)))]
    (test-assert "skeleton of a 2D array is a 2D array"
      (is-a? m (multiarray <int> 2)))
    (test-equal "skeleton of 2D array consists of long integer variables and an unsigned long integer"
      (list <long> <long> <long> <long> <ulong>) (map class-of (content (class-of m) m)))
    (test-equal "2D array skeleton is based on five variables"
      (make-list 5 <var>) (map class-of (map get (content (class-of m) m)))))
(test-end "skeleton of array expression")

(test-begin "type inference")
  (test-eq "determine type of parameter"
    <sint> (type (parameter <sint>)))
  (test-eq "determine type of sequence"
    (sequence <ubyte>) (type (parameter (sequence <ubyte>))))
  (test-eq "plus operation coerces return type correctly"
    <int> (type (make-function + coerce list (list (parameter <usint>) (parameter <byte>)))))
  (test-eq "coerce sequence and scalar"
    (sequence <sint>) (type (make-function + coerce list (list (parameter (sequence <ubyte>)) (parameter <sint>)))))
  (test-eq "coerce two sequence types"
    (sequence <usint>) (type (make-function + coerce list (list (parameter (sequence <ubyte>)) (parameter (sequence <usint>))))))
(test-end "type inference")
(test-end "aiscm expression")
