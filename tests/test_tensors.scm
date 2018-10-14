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
    (pointer <byte>) (class-of (car iter)))
  (test-eq "get pointer of tensor (start value)"
    (memory t) (cadr iter))
  (test-eqv "get stride of tensor (increment)"
    3 ((get (caddr iter)) #f))
  (test-eq "loop body has one dimension less"
    1 (length (shape (cadddr iter))))
  (test-eq "loop body uses rebased tensor"
    (car iter) (memory (cadddr iter))))


(define-tensor (rebuild x) (tensor i (get x i)))
(define-tensor (rebuild-2d x) (tensor j (tensor i (get (get x j) i))))
(define-tensor (transpose x) (tensor j (tensor i (get (get x i) j))))
;(define-tensor (index-array n) (tensor (i n) i))
(test-group "array indexing"
  (test-equal "rebuild array"
    '(2 3 5) (to-list (rebuild (arr 2 3 5))))
  (test-equal "rebuild 2D array"
    '((2 3 5) (3 5 7)) (to-list (rebuild (arr (2 3 5) (3 5 7)))))
  (test-equal "transpose 2D array"
    '((2 3) (3 5) (5 7)) (to-list (transpose (arr (2 3 5) (3 5 7)))))
  (test-skip 1)
  (test-equal "index array"
    '(0 1 2) (to-list (index-array 3))))

(test-end "aiscm tensors")
