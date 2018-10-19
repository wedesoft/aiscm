;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm core)
             (aiscm tensors))


(test-begin "aiscm tensors")

(define-tensor (trivial x) x)
(define-tensor (second a b) b)
(test-group "trivial tensors"
  (test-eqv "pass through integer"
    5 (trivial 5))
  (test-eqv "pass through second argument"
    3 (second 2 3))
  (test-eqv "map to 32 bit integers"
    1234 (trivial 1234))
  (test-equal "pass through array"
    '(2 3 5) (to-list (trivial (arr 2 3 5)))))

(define one (make <int> #:value (const 1)))
(define two (make <int> #:value (const 2)))
(define three (make <int> #:value (const 3)))
(define p (make (pointer <byte>) #:value (const 1234)))
(define a (llvmarray p p (llvmlist three) (llvmlist one)))
(define b (llvmarray p p (llvmlist three two) (llvmlist one three )))

(test-group "convert array to tensor"
  (test-eqv "pass-through integer"
    42 (get (expression->tensor (make <int> #:value 42))))
  (test-eq "convert array to function of index"
    <functional> (class-of (expression->tensor a)))
  (test-eq "use lookup object"
    <lookup> (class-of (term (expression->tensor a))))
  (test-eq "declare index"
    <index> (class-of (index (expression->tensor a))))
  (let [(t (expression->tensor a))]
    (test-eq "use same index for function and lookup"
      (index t) (index (term t))))
  (test-eqv "lookup uses stride of array"
    1 ((get (stride (term (expression->tensor a)))) #f))
  (test-eq "lookup contains element accessor"
    (elementary <byte>) (class-of (term (term (expression->tensor a)))))
  (test-eqv "lookup contains element accessor with same memory"
    1234 ((get (memory (term (term (expression->tensor a))))) #f))
  (test-eq "2D array contains function of index at second level"
    <functional> (class-of (term (expression->tensor b))))
  (test-equal "get shape of 1D tensor"
    '(3) (map (lambda (value) ((get value) #f)) (shape (expression->tensor a))))
  (test-equal "get shape of 2D tensor"
    '(3 2) (map (lambda (value) ((get value) #f)) (shape (expression->tensor b))))
  (test-equal "get typecode of 1D tensor"
    <byte> (typecode (expression->tensor a))))

(define t (expression->tensor b))
(define iter (tensor-iterate t))
(test-group "tensor iteration"
  (test-eq "project 1D array"
    (elementary <byte>) (class-of (project (expression->tensor a))))
  (test-eq "project 2D array twice"
    (elementary <byte>) (class-of (project (project (expression->tensor b)))))
  (test-eq "create pointer for iterating"
    (pointer <byte>) (class-of (caar iter)))
  (test-eq "get pointer of tensor (start value)"
    (memory t) (caadr iter))
  (test-eqv "get stride of tensor (increment)"
    3 ((get (caaddr iter)) #f))
  (test-eq "return projected expression"
    (elementary <byte>) (class-of (project (cadddr iter)))))


(define-tensor (rebuild x) (tensor i (get x i)))
(define-tensor (rebuild-2d x) (tensor j (tensor i (get (get x j) i))))
(define-tensor (transpose x) (tensor j (tensor i (get (get x i) j))))
(define-tensor (index-array n) (tensor (i n) i))
(define-tensor (index-y n m) (tensor (j m) (tensor (i n) j)))
(define-tensor (index-x n m) (tensor (j m) (tensor (i n) i)))

(test-group "array indexing"
  (test-equal "rebuild array"
    '(2 3 5) (to-list (rebuild (arr 2 3 5))))
  (test-equal "rebuild 2D array"
    '((2 3 5) (3 5 7)) (to-list (rebuild (arr (2 3 5) (3 5 7)))))
  (test-equal "transpose 2D array"
    '((2 3) (3 5) (5 7)) (to-list (transpose (arr (2 3 5) (3 5 7)))))
  (test-equal "index array"
    '(0 1 2) (to-list (index-array 3)))
  (test-equal "2D index array with row index"
    '((0 0 0) (1 1 1)) (to-list (index-y 3 2)))
  (test-equal "2D index array with column index"
    '((0 1 2) (0 1 2)) (to-list (index-x 3 2))))

(define-tensor (plus a b) (+ a b))
(define-tensor (plus-1d a b) (tensor i (+ (get a i) (get b i))))
(define-tensor (plus-2d-1d a b) (tensor j (tensor i (+ (get (get a j) i) (get b j)))))
(define-tensor (plus-1d-2d a b) (tensor j (tensor i (+ (get a j) (get (get b j) i)))))
(define-tensor (plus-1d-scalar a b) (tensor i (+ (get a i) b)))
(define-tensor (plus-scalar-1d a b) (tensor i (+ a (get b i))))
(test-group "binary operations"
  (test-equal "scalar plus"
    5 (plus 2 3))
  (test-equal "plus for 1D array tensor"
    '(5 8 12) (to-list (plus-1d (arr 2 3 5) (arr 3 5 7))))
  (test-equal "simply add two arrays"
    '(5 8 12) (to-list (plus (arr 2 3 5) (arr 3 5 7))))
  (test-equal "add 2D and 1D tensor"
    '((7 8)) (to-list (plus-2d-1d (arr (2 3)) (arr 5))))
  (test-equal "add 1D and 2D tensor"
    '((7 8)) (to-list (plus-1d-2d (arr 5) (arr (2 3)))))
  (test-equal "add 1D and scalar tensor"
    '(3 4 6) (to-list (plus-1d-scalar (arr 2 3 5) 1)))
  (test-equal "add scalar and 1D tensor"
    '(3 4 6) (to-list (plus-scalar-1d 1 (arr 2 3 5))))
  (test-equal "add composite and 1D tensor"
    (list (rgb 3 4 5) (rgb 4 5 6) (rgb 6 7 8)) (to-list (plus-scalar-1d (rgb 1 2 3) (arr 2 3 5))))
  (test-equal "add 1D tensor and composite"
    (list (rgb 3 4 5) (rgb 4 5 6) (rgb 6 7 8)) (to-list (plus-1d-scalar (arr 2 3 5) (rgb 1 2 3)))))

(test-end "aiscm tensors")
