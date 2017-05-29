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
             (aiscm pointer)
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

(test-begin "decompose parameters into elementary native types")
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
(test-end "decompose parameters into elementary native types")

(test-begin "array parameters")
  (let* [(s  (skeleton (sequence <int>)))
         (sx (parameter s))
         (i  (var <long>))]
    (test-eq "sequence parameter maintains pointer"
      (value s) (value (delegate (delegate sx))))
    (test-eq "index of parameter and index of parameters content should match"
      (index sx) (index (delegate sx)))
    (test-eq "sequence parameter should maintain dimension"
      (dimension s) (get (delegate (dimension sx))))
    (test-eq "sequence parameter should maintain stride"
      (stride s) (get (delegate (stride (delegate sx)))))
    (test-eq "sequence parameter maintains type"
      (sequence <int>) (type sx))
    (test-eq "substitution should replace the lookup index"
      i (index (subst (delegate sx) (index sx) i)))
    (test-eq "retrieving an element by index should replace with the index"
      i (index (get sx i)))
    (test-assert "projected 1D array tensor should contain pointer"
      (is-a? (delegate (project sx)) (pointer <int>))))
(test-end "array parameters")

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

(test-begin "array parameter manipulation")
  (let* [(m  (skeleton (multiarray <int> 2)))
         (mx (parameter m))
         (i  (var <long>))
         (j  (var <long>))]
    (test-equal "2D array parameter should maintain the shape"
      (shape m) (map (compose get delegate) (shape mx)))
    (test-equal "2D array parameter should maintain the strides"
      (strides m) (map (compose get delegate) (strides mx)))
    (test-equal "first index of parameter should have a match"
      (index mx) (index (delegate (delegate mx))))
    (test-equal "second index of parameter should have a match"
      (index (delegate mx)) (index (delegate (delegate (delegate mx)))))
    (test-eq "subst should allow replacing first index"
      i (index (subst (delegate (delegate mx)) (index mx) i)))
    (test-eq "subst should allow replacing second index"
      i (index (delegate (subst (delegate (delegate mx)) (index (delegate mx)) i))))
    (test-eq "replacing the second index should maintain the first one"
      (index mx) (index (subst (delegate (delegate mx)) (index (delegate mx)) i)))
    (test-eq "retrieving an element should replace with the index"
      i (index (delegate (get mx i))))
    (let [(tr (indexer i (indexer j (get (get mx j) i) (cadr (shape mx))) (car (shape mx))))]
      (test-equal "swap dimensions when transposing"
        (list (dimension mx) (dimension (project mx))) (list (dimension (project tr)) (dimension tr)))
      (test-equal "swap strides when transposing"
        (list (stride mx) (stride (project mx))) (list (stride (project tr)) (stride tr)))))
(test-end "array parameter manipulation")

(test-begin "tensor dimensions")
  (test-assert "dimension hint is false initially"
    (not (dimension-hint (var <long>))))
  (let [(s (parameter (sequence <int>)))
        (i (var <long>))]
    (get s i)
    (test-eq "dimension hint is defined when using an index"
      (dimension s) (dimension-hint i)))
(test-end "tensor dimensions")

(test-begin "shape of expression")
  (let [(m (parameter (multiarray <int> 2)))
        (c (parameter <byte>))]
    (test-equal "shape of unary function expression is shape of argument"
      (shape m) (shape (make-function ~ coerce list (list m))))
    (test-equal "shape of scalar plus array expression"
      (shape m) (shape (make-function + coerce list (list c m))))
    (test-equal "shape of array plus scalar expression"
      (shape m) (shape (make-function + coerce list (list m c)))))
(test-end "shape of expression")

(test-begin "dimension of expression")
  (let [(m (parameter (multiarray <int> 2)))]
    (test-equal "dimension of unary function expression is dimension of argument"
      (dimension m) (dimension (make-function ~ coerce list (list m)))))
(test-end "dimension of expression")
(test-end "aiscm expression")
