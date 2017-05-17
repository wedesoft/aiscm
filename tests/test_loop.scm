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
             (aiscm loop)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm asm)
             (aiscm variable)
             (aiscm expression)
             (aiscm program))


(test-begin "aiscm loop")
(test-begin "1D loop with multiple iterators")
  (let* [(s (parameter (sequence <ubyte>)))
         (t (multi-loop s))
         (l (car (loop-details t)))]
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (iterator l) <var>))
    (test-eq "loop iterator is a 64 bit integer"
      <long> (typecode (iterator l)))
    (test-assert "tensor layer of sequence has an iterator"
      (is-a? (step l) <var>))
    (test-eq "step size is a 64 bit integer"
      <long> (typecode (step l)))
    (test-eq "body of trivial tensor is a (scalar) parameter"
      <param> (class-of (body t)))
    (test-eq "body of trivial tensor is rebased on iterator"
      (iterator l) (value (body t)))
    (test-eq "typecode of unsigned byte tensor is unsigned byte"
      <ubyte> (typecode l))
    (test-eq "typecode of short integer tensor is short integer"
      <sint> (typecode (car (loop-details (multi-loop (parameter (sequence <sint>)))))))
    (test-equal "stride of tensor is stride of input array"
      (stride (delegate s)) (stride l))
    (test-equal "base of tensor is base pointer of input array"
      (value s) (base l)))
(test-end "1D loop with multiple iterators")

(test-begin "2D loop with multiple iterators")
  (let* [(m (parameter (multiarray <ubyte> 2)))
         (i (var <long>))
         (j (var <long>))
         (t (dim j (dim i (make-function + coerce list (list (get (get m i) j))))))]
    (test-assert "tensor loop preserves inner index"
      (is-a? (body (multi-loop m)) <indexer>))
    (test-eq "inner index is second index of 2D array"
      (index (delegate m)) (index (body (multi-loop m))))
    (test-eq "inner dimension is second dimension of 2D array"
      (dimension (delegate m)) (dimension (body (multi-loop m))))
    (test-assert "preserve loop details when skipping indices"
      (is-a? (car (loop-details (multi-loop m))) <loop-detail>))
    (test-assert "body of 2D tensor drops inner lookup"
      (is-a? (delegate (body (multi-loop m))) <lookup>))
    (test-skip 1); + operator not available here
    (test-assert "tensor loop should preserve 2nd index of transposed array"
      (is-a? (delegate (body (multi-loop t))) <lookup>)))
(test-end "2D loop with multiple iterators")

(test-begin "zero-dimensional loop")
  (let [(v (parameter <int>))
        (i (var <long>))]
    (test-assert "loop code for scalar tensor is empty"
      (null? (loop-details (multi-loop v))))
    (test-eq "body of scalar tensor is itself"
      v (body (multi-loop v)))
    (test-eq "scalar tensor ignores indices"
      v (body (multi-loop v i))))
(test-end "zero-dimensional loop")

(test-begin "loop code")
  (let* [(iterator   (var <long>))
         (step       (var <long>))
         (stride     (parameter <long>))
         (base       (var <long>))
         (loop-ubyte (make <loop-detail> #:typecode <ubyte> #:iterator iterator #:step step #:stride stride #:base base))
         (loop-usint (make <loop-detail> #:typecode <usint> #:iterator iterator #:step step #:stride stride #:base base))]
  (test-equal "setup of array loop should define increment and initialise pointer"
    (list (IMUL step (value stride) 1) (MOV iterator base))
    (loop-setup loop-ubyte))
  (test-equal "setup of array loop adjust the step size according to the array type"
    (list (IMUL step (value stride) 2) (MOV iterator base))
    (loop-setup loop-usint))
  (test-equal "a loop increment should increment the loop iterator"
    (list (ADD iterator step))
    (loop-increment loop-ubyte)))
(test-end "loop code")
(test-end "aiscm loop")
