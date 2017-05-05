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
             (srfi srfi-1)
             (oop goops)
             (aiscm sequence)
             (aiscm element)
             (aiscm variable)
             (aiscm bool)
             (aiscm int)
             (aiscm obj)
             (aiscm pointer)
             (aiscm jit)
             (aiscm util))


(test-begin "aiscm sequence")

(define s1 (make (sequence <sint>) #:size 3))
(define s2 (make (sequence <sint>) #:size 3))
(define s3 (make (sequence <sint>) #:size 3))
(set s1 0 2) (set s1 1 3) (set s1 2 5)
(define a (make <var> #:type <long> #:symbol 'a))
(define b (make <var> #:type <long> #:symbol 'b))
(define c (make <var> #:type <long> #:symbol 'c))
(define d (make <var> #:type <long> #:symbol 'd))
(define e (make <var> #:type <long> #:symbol 'e))

(test-equal "Query element type of sequence class"
  <sint> (typecode (sequence <sint>)))
(test-equal "equality of classes"
  (sequence <sint>) (sequence <sint>))
(test-eqv "Query size of sequence"
  3 (size s1))
(test-equal "Query typecode of sequence"
  <sint> (typecode s1))
(test-equal "Query element type of sequence"
  (pointer <sint>) (project (sequence <sint>)))
(test-eqv "Write value to sequence"
  9 (begin (set s2 2 9) (get s2 2)))
(test-eqv "Check number of dimensions of sequence type"
  1 (dimensions (sequence <sint>)))
(test-eqv "Query dimension of sequence"
  3 (dimension s1))
(test-equal "Query shape of sequence"
  '(3) (shape s1))
(test-equal "determine default strides of a sequence"
  '(1 2 6) (default-strides '(2 3 5)))
(test-equal "Query stride of sequence"
  1 (stride s1))
(test-equal "query strides of sequence"
  '(1) (strides s1))
(test-equal "Convert sequence to list"
  '(2 3 5) (to-list s1))
(test-assert "Make empty sequence"
  (make (sequence <int>) #:size 0))
(test-equal "Class name of 16-bit integer sequence"
  '<sequence<int<16,signed>>> (class-name (sequence <sint>)))
(test-equal "Write empty sequence"
  "#<sequence<int<32,signed>>>:\n()" (call-with-output-string (lambda (port) (write (make (sequence <int>) #:size 0) port))))
(test-equal "Write sequence object"
  "#<sequence<int<16,signed>>>:\n(2 3 5)" (call-with-output-string (lambda (port) (write s1 port))))
(test-equal "Write longer sequence object"
  "#<sequence<int<8,unsigned>>>:\n(100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 ...)"
  (call-with-output-string (lambda (port) (write (to-array (make-list 40 100)) port))))
(test-equal "Write sequence object made from variables"
  "#<<sequence<int<8,signed>>> c:<int<64,signed>> (a:<int<64,signed>>) (b:<int<64,signed>>)>"
  (let [(s (make (sequence <byte>) #:value c #:shape (list a) #:strides (list b)))]
    (call-with-output-string (lambda (port) (write s port)))))
(test-equal "Typecode of converted list of unsigned bytes"
  <ubyte> (typecode (to-array '(1 2 3))))
(test-equal "Typecode of converted list of signed bytes"
  <byte> (typecode (to-array '(1 -1))))
(test-eqv "storage size of short integer sequence"
  6 (size-of (make (sequence <sint>) #:size 3)))
(test-eqv "Size of converted list"
  3 (size (to-array '(1 2 3))))
(test-equal "Assignment list to sequence"
  '(2 3 5) (begin (set s3 '(2 3 5)) (to-list s3)))
(test-equal "Assignment number to sequence"
  '(3 3 3) (begin (set s3 3) (to-list s3)))
(test-equal "Return value of assignment to sequence"
  '(2 3 5) (set s3 '(2 3 5)))
(test-equal "Convert list of integers to multiarray and back"
  '(2 4 8) (to-list (to-array '(2 4 8))))
(test-equal "Convert list of boleans to multiarray and back"
  '(#t #f #t) (to-list (to-array '(#t #f #t))))
(test-equal "Short form for specifying sequences"
  '(2 3 5) (to-list (sequence 2 3 (+ 2 3))))
(test-equal "Short short form for specifying sequences"
  '(2 3 5) (to-list (seq 2 3 (+ 2 3))))
(test-equal "Checking content of short form for integer sequence"
  '(2 3 5) (to-list (seq <int> 2 3 5)))
(test-equal "Checking type of short form for integer sequence "
  <int> (typecode (seq <int> 2 3 5)))
(test-equal "Selection of fitting datatype for signed-unsigned combinations"
  '(-1 128) (to-list (seq -1 128)))
(test-equal "Short form for specifying arrays"
  '((2 3) (5 7)) (to-list (arr (2 3) (5 7))))
(test-equal "Checking content of short form for specifying integer arrays"
  '((2 3) (5 7)) (to-list (arr <int> (2 3) (5 7))))
(test-equal "Checking type of short form for specifying integer arrays"
  <int> (typecode (arr <int> (2 3) (5 7))))
(test-equal "Convert list of integers to integer array"
  <int> (typecode (to-array <int> '(1 2 3))))
(test-equal "Convert list of integers to integer array and back"
  '(1 2 3) (to-list (to-array <int> '(1 2 3))))
(test-equal "Write 2D array"
  "#<sequence<sequence<int<8,unsigned>>>>:\n((1 2 3)\n (4 5 6))"
  (call-with-output-string (lambda (port) (write (arr (1 2 3) (4 5 6)) port))))
(test-equal "Write 2D array with large first dimension"
  "#<sequence<sequence<int<8,unsigned>>>>:\n((100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 ...))"
  (call-with-output-string (lambda (port) (write (to-array (list (make-list 40 100))) port))))
(test-equal "Write 2D array with large second dimension"
  "#<sequence<sequence<int<8,unsigned>>>>:\n((1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n ..."
  (call-with-output-string (lambda (port) (write (to-array (make-list 11 '(1))) port))))
(test-equal "Write 3D array"
  "#<sequence<sequence<sequence<int<8,unsigned>>>>>:\n(((1 1)\n  (1 1))\n ((1 1)\n  (1 1)))"
  (call-with-output-string (lambda (port) (write (to-array (make-list 2 (make-list 2 '(1 1)))) port))))
(test-equal "Write 4x3x2 array"
  "#<sequence<sequence<sequence<int<8,unsigned>>>>>:\n(((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n ..." (call-with-output-string (lambda (port) (write (to-array (make-list 4 (make-list 3 '(1 1)))) port))))
(test-equal "Write 5x3x2 array"
  "#<sequence<sequence<sequence<int<8,unsigned>>>>>:\n(((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n  (1 1)\n  (1 1))\n ((1 1)\n ..." (call-with-output-string (lambda (port) (write (to-array (make-list 5 (make-list 3 '(1 1)))) port))))
(test-equal "Coercion of sequences"
  (sequence <int>) (coerce <int> (sequence <sint>)))
(test-equal "Coercion of sequences"
  (sequence <int>) (coerce (sequence <int>) <byte>))
(test-equal "Coercion of sequences"
  (sequence <int>) (coerce (sequence <int>) (sequence <byte>)))
(test-equal "Coercion of multi-dimensional arrays"
  (multiarray <int> 2) (coerce (multiarray <int> 2) <int>))
(test-equal "Class name of 16-bit integer 2D array"
  '<sequence<sequence<int<16,signed>>>> (class-name (sequence (sequence <sint>))))
(test-equal "Multi-dimensional array is the same as a sequence of sequences"
  (multiarray <sint> 2) (sequence (sequence (integer 16 signed))))
(test-equal "Get element type of 2D array"
  (sequence <sint>) (project (multiarray <sint> 2)))
(test-assert "Shape of arbitrary object is empty list"
  (null? (shape 1)))
(test-equal "Shape of flat list"
  '(3) (shape '(1 2 3)))
(test-equal "Shape of nested list"
  '(3 2) (shape '((1 2 3) (4 5 6))))
(test-equal "Query shape of multi-dimensional array"
  '(5 4 3) (shape (make (multiarray <int> 3) #:shape '(5 4 3))))
(test-equal "Query strides of multi-dimensional array"
  '(1 5 20) (strides (make (multiarray <int> 3) #:shape '(5 4 3))))
(test-eqv "Query last stride of multi-dimensional array"
  20 (stride (make (multiarray <int> 3) #:shape '(5 4 3))))
(test-equal "Query last dimension of multi-dimensional array"
  3 (dimension (make (multiarray <int> 3) #:shape '(5 4 3))))
(test-equal "'get' without additional arguments should return the sequence itself"
  '(1 2 3) (to-list (get (seq 1 2 3))))
(test-equal "Content of converted 2D array"
  '((1 2 3) (4 5 6)) (to-list (arr (1 2 3) (4 5 6))))
(test-equal "Getting row of 2D array"
  '(4 5 6) (to-list (get (arr (1 2 3) (4 5 6)) 1)))
(test-equal "Getting element of 2D array with one call to 'get'"
  2 (get (arr (1 2 3) (4 5 6)) 1 0))
(test-equal "Getting element of 2D array with two calls to 'get'"
  2 (get (get (arr (1 2 3) (4 5 6)) 0) 1))
(test-equal "Setting an element in a 2D array"
  42 (let [(m (arr (1 2 3) (4 5 6)))] (set m 1 0 42) (get m 1 0)))
(test-equal "Setting a row in a 2D array"
  '((1 2) (5 6)) (let [(m (arr (1 2) (3 4)))] (set m 1 '(5 6)) (to-list m)))
(test-equal "Drop 2 elements of an array"
  '(3 4 5) (to-list (dump 2 (seq 1 2 3 4 5))))
(test-equal "Drop rows and columns from 2D array"
  '((2 3) (5 6)) (to-list (dump '(1 0) (arr (1 2 3) (4 5 6)))))
(test-equal "Drop elements from a 3D array"
  '(3 3 3) (shape (dump '(1 2) (make (multiarray <int> 3) #:shape '(3 4 5)))))
(test-equal "project 2D array"
  '(1 2 3) (to-list (project (arr (1 2 3) (4 5 6)))))
(test-equal "Crop an array down to 3 elements"
  '(1 2 3) (to-list (crop 3 (seq 1 2 3 4))))
(test-equal "Crop 2D array to size 2x1"
  '((1 2)) (to-list (crop '(2 1) (arr (1 2 3) (4 5 6)))))
(test-equal "Crop 3D array"
  '(3 1 2) (shape (crop '(1 2) (make (multiarray <int> 3) #:shape '(3 4 5)))))
(test-equal "Rolling an array should cycle the indices"
  '(((1)) ((2))) (to-list (roll (arr ((1 2))))))
(test-equal "Unrolling an array should reverse cycle the indices"
  '(((1) (2))) (to-list (unroll (arr ((1 2))))))
(test-equal "Downsampling by 2 with phase 0"
  '(1 3 5) (to-list (downsample 2 (seq 1 2 3 4 5))))
(test-equal "Downsampling of 2D array"
  '((1) (3) (5)) (to-list (downsample 2 (arr (1) (2) (3) (4) (5)))))
(test-equal "1-2 Downsampling of 2D array"
  '((1 2 3)) (to-list (downsample '(1 2) (arr (1 2 3) (4 5 6)))))
(test-equal "2-1 Downsampling of 2D array"
  '((1 3) (4 6)) (to-list (downsample '(2 1) (arr (1 2 3) (4 5 6)))))
(test-equal "Downsample 3D array"
  '(6 3 2) (shape (downsample '(2 3) (make (multiarray <int> 3) #:shape '(6 6 6)))))
(test-equal "'unbuild' for an array should return size and stride"
  '(3 1) (take (unbuild (class-of s1) s1) 2))
(test-equal "'unbuild' for a 2D array should return shape and strides"
  '(4 6 6 1) (take (unbuild (multiarray <byte> 2) (make (multiarray <byte> 2) #:shape '(6 4))) 4))
(let [(roundtrip (build (class-of s1) (unbuild (class-of s1) s1)))]
  (test-equal "reconstruct sequence from components"
  '(2 3 5) (to-list roundtrip))
  (test-eqv "size of sequence memory is set correctly when reconstructing"
  6 (get-size (slot-ref roundtrip 'value))))
(let* [(m         (arr <sint> (1 2 3) (4 5 6)))
       (roundtrip (build (class-of m) (unbuild(class-of m) m)))]
  (test-equal "reconstruct 2D array from components"
  '((1 2 3) (4 5 6)) (to-list roundtrip))
  (test-eqv "size of array memory is set correctly when reconstructing"
  12 (get-size (slot-ref roundtrip 'value))))
(test-equal "'content' should convert dimension and stride to integer elements"
  (list <long> <long>) (map class-of (take (content (sequence <int>) (skeleton (sequence <int>))) 2)))
(test-equal "'content' should return pointer object as an integer in addition to dimension and stride"
  <ulong> (class-of (last (content (sequence <int>) (skeleton (sequence <int>))))))
(let [(s (parameter (sequence <int>)))]
  (test-eq "content of sequence parameter contains dimension"
    (dimension s) (car (content (sequence <int>) s)))
  (test-eq "content of sequence parameter contains stride"
    (stride s) (cadr (content (sequence <int>) s)))
  (test-eq "content of sequence contains pointer"
    (get (delegate (project s))) (get (delegate (caddr (content (sequence <int>) s))))))
(test-assert "Check signed-ness of sequence"
  (signed? (seq -1 1)))
(test-assert "integer sequence memory is pointerless"
  (pointerless? (sequence <int>)))
(test-assert "object sequence memory is not pointerless"
  (not (pointerless? (sequence <obj>))))
(test-equal "sequence of objects is filled with boolean false"
  '(#f #f #f) (to-list (make (sequence <obj>) #:size 3)))
(test-equal "2D array of objects is filled with boolean false"
  '((#f #f) (#f #f)) (to-list (make (multiarray <obj> 2) #:shape '(2 2))))

(test-end "aiscm sequence")
