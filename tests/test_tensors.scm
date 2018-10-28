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
(define-tensor (two-times-index n) (tensor (i n) (* 2 i)))
(define-tensor (index-times-two n) (tensor (i n) (* i 2)))

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
    '((0 1 2) (0 1 2)) (to-list (index-x 3 2)))
  (test-equal "Multiply two with index"
    '(0 2 4) (to-list (two-times-index 3)))
  (test-equal "Multiply index with two"
    '(0 2 4) (to-list (index-times-two 3))))

(define-tensor (plus a b) (+ a b))
(define-tensor (plus-1d a b) (tensor i (+ (get a i) (get b i))))
(define-tensor (plus-2d-1d a b) (tensor j (tensor i (+ (get (get a j) i) (get b j)))))
(define-tensor (plus-1d-2d a b) (tensor j (tensor i (+ (get a j) (get (get b j) i)))))
(define-tensor (plus-1d-scalar a b) (tensor i (+ (get a i) b)))
(define-tensor (plus-scalar-1d a b) (tensor i (+ a (get b i))))
(define-tensor (minus a b) (- a b))
(define-tensor (product a b) (* a b))
(define-tensor (division a b) (/ a b))
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
    (list (rgb 3 4 5) (rgb 4 5 6) (rgb 6 7 8)) (to-list (plus-1d-scalar (arr 2 3 5) (rgb 1 2 3))))
  (test-equal "simply add array and scalar"
    '(3 4 6) (to-list (plus (arr 2 3 5) 1)))
  (test-equal "simply add scalar and array"
    '(3 4 6) (to-list (plus 1 (arr 2 3 5))))
  (test-equal "simply add 2D and 1D array"
    '((7 8)) (to-list (plus (arr (2 3)) (arr 5))))
  (test-equal "simply add 1D and 2D array"
    '((7 8)) (to-list (plus (arr 5) (arr (2 3)))))
  (test-equal "tensor minus"
    '(2) (to-list (minus (arr 5) (arr 3))))
  (test-equal "tensor product"
    '(15) (to-list (product (arr 5) (arr 3))))
  (test-equal "tensor division"
    '(2) (to-list (division (arr 6) (arr 3)))))

(define-tensor (negative a) (- a))
(define-tensor (negative-1d a) (tensor i (- (get a i))))
(define-tensor (zero-minus a) (- 0 a))
(define-tensor (zero-minus-1d a) (tensor i (- 0 (get a i))))
(define-tensor (plus-one a) (+ a 1))
(define-tensor (plus-one-1d a) (tensor i (+ (get a i) 1)))
(define-tensor (negation a) (~ a))
(test-group "unary operation"
  (test-equal "scalar minus"
    3 (negative -3))
  (test-equal "minus for 1D array tensor"
    '(-2 3 -5) (to-list (negative-1d (arr 2 -3 5))))
  (test-equal "simply negate array"
    '(-2 3 -5) (to-list (negative (arr 2 -3 5))))
  (test-equal "compute zero minus array"
    '(-2 3 -5) (to-list (zero-minus (arr 2 -3 5))))
  (test-equal "compute zero minus tensor"
    '(-2 3 -5) (to-list (zero-minus-1d (arr 2 -3 5))))
  (test-equal "compute array plus one"
    '(3 4 6) (to-list (plus-one (arr 2 3 5))))
  (test-equal "compute tensor plus one"
    '(3 4 6) (to-list (plus-one-1d (arr 2 3 5))))
  (test-equal "bitwise negative of array"
    '(253 252 250) (to-list (negation (arr 2 3 5)))))

(define-tensor (sum-1d a) (sum-over i (get a i)))
(define-tensor (sum-2d a) (sum-over i (sum-over j (get (get a j) i))))
(define-tensor (sum-cols a) (tensor i (sum-over j (get (get a j) i))))
(define-tensor (sum-rows a) (tensor i (sum-over j (get (get a i) j))))
(define-tensor (dot a b) (tensor j (sum-over k (* (get (get a j) k) (get b k)))))
(define-tensor (get-two a) (tensor j (tensor i (get a j i))))
(define-tensor (prod-1d a) (product-over i (get a i)))

(test-group "tensor reductions"
  (test-equal "Sum elements of 1Darray"
    10 (sum-1d (arr 2 3 5)))
  (test-equal "Sum elements of 2D array"
    25 (sum-2d (arr (2 3 5) (3 5 7))))
  (test-equal "Sum each column"
    '(5 8 12) (to-list (sum-cols (arr (2 3 5) (3 5 7)))))
  (test-equal "Sum each row"
    '(10 15) (to-list (sum-rows (arr (2 3 5) (3 5 7)))))
  (test-equal "1D sum over 2D array"
    '(5 8 12) (to-list (sum-1d (arr (2 3 5) (3 5 7)))))
  (test-equal "Dot product"
    '(38 56) (to-list (dot (arr (2 3 5) (3 5 7)) (arr 2 3 5))))
  (test-equal "Dot product of arrays"
    '((38) (56)) (to-list (dot (arr (2 3 5) (3 5 7)) (arr (2) (3) (5)))))
  (test-equal "Get with multiple indices"
    '((2 5) (3 7)) (to-list (get-two (arr (2 3) (5 7)))))
  (test-equal "Multiply elements of 1Darray"
    30 (prod-1d (arr 2 3 5))))

(define-tensor (tensor-sqrt a) (sqrt a))
(define-tensor (tensor-sin a) (sin a))
(define-tensor (tensor-cos a) (cos a))
(define-tensor (tensor-tan a) (tan a))
(define-tensor (tensor-asin a) (asin a))
(define-tensor (tensor-acos a) (acos a))
(define-tensor (tensor-atan a) (atan a))
(define-tensor (tensor-minor a b) (minor a b))
(define-tensor (tensor-major a b) (major a b))
(define-tensor (tensor-pow a b) (pow a b))
(define-tensor (tensor-atan a b) (atan a b))

(test-group "various functions"
  (test-assert "Tensor square root"
    (tensor-sqrt (arr 2 3 5)))
  (test-eqv "Square root maps to float"
    2.0 (get (tensor-sqrt (arr 4)) 0))
  (test-assert "Tensor sinus"
    (tensor-sin (arr 2 3 5)))
  (test-assert "Tensor cosinus"
    (tensor-cos (arr 2 3 5)))
  (test-assert "Tensor tangens"
    (tensor-tan (arr 2 3 5)))
  (test-assert "Tensor arcus sinus"
    (tensor-asin (arr 0)))
  (test-assert "Tensor arcus cosinus"
    (tensor-acos (arr 0)))
  (test-assert "Tensor arcus tangens"
    (tensor-atan (arr 0)))
  (test-assert "Tensor minor"
    (tensor-minor (arr 2) (arr 3)))
  (test-assert "Tensor major"
    (tensor-major (arr 2) (arr 3)))
  (test-assert "Tensor exponentiation"
    (tensor-pow (arr 2) (arr 3)))
  (test-assert "Tensor arcus tangens 2"
    (tensor-atan (arr 0) (arr 1))))

(define-tensor (negative-i n) (tensor (i n) (- i)))
(define-tensor (plus-i n) (tensor (i n) (+ i i)))
(define-tensor (arr-plus-i a) (tensor i (+ (get a i) i)))
(define-tensor (sum-i n) (sum-over (i n) i))
(define-tensor (min-i a) (min-over i (get a i)))
(define-tensor (max-i a) (max-over i (get a i)))
(test-group "operations with indices"
  (test-equal "negative index"
    '(0 -1 -2) (to-list (negative-i 3)))
  (test-equal "add indices"
    '(0 2 4) (to-list (plus-i 3)))
  (test-equal "add array element and index"
    '(2 4 7) (to-list (arr-plus-i (arr 2 3 5))))
  (test-eqv "sum over range of indices"
    3 (sum-i 3))
  (test-eqv "minimum of array"
    2 (min-i (arr 2 3 5)))
  (test-eqv "maximum of array"
    5 (max-i (arr 2 3 5))))

(test-end "aiscm tensors")
