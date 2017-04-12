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
(use-modules (oop goops)
             (srfi srfi-26)
             (aiscm asm)
             (aiscm util)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm rgb)
             (aiscm obj)
             (aiscm pointer)
             (aiscm sequence))


(test-begin "aiscm jit7")

(define ctx (make <context>))
(define i (var <long>))
(define j (var <long>))

(let* [(s  (skeleton (sequence <int>)))
       (sx (parameter s))]
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
(let* [(m  (skeleton (multiarray <int> 2)))
       (mx (parameter m))]
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
  (let [(tr (indexer (car (shape mx)) i (indexer (cadr (shape mx)) j (get (get mx j) i))))]
    (test-equal "swap dimensions when transposing"
      (list (dimension mx) (dimension (project mx))) (list (dimension (project tr)) (dimension tr)))
    (test-equal "swap strides when transposing"
      (list (stride mx) (stride (project mx))) (list (stride (project tr)) (stride tr)))))
(let [(s (seq <int> 2 3 5))
      (t (seq <int> 3 5 7))
      (m (arr <int> (2 3 5) (7 11 13) (17 19 23)))
      (r (arr <int> (2 3 5) (7 11 13)))]
  (test-skip 1)
  (test-equal "switch dimensions of a 2D tensor"
    '((2 7 17) (3 11 19) (5 13 23))
    (to-list ((jit ctx (list (class-of m))
                   (lambda (m) (indexer (car (shape m)) i (indexer (cadr (shape m)) j (get (get m j) i)))))
              m)))
  (test-equal "tensor macro provides local variable"
    (to-list s) (to-list ((jit ctx (list (class-of s)) (lambda (s) (tensor (dimension s) k (get s k)))) s)))
  (test-skip 2)
  (test-equal "switch dimensions of a non-square 2D tensor"
    '((2 7) (3 11) (5 13))
    (to-list ((jit ctx (list (class-of r))
                   (lambda (r) (indexer (car (shape r)) i (indexer (cadr (shape r)) j (get (get r j) i)))))
              r)))
  (test-equal "tensor expression for element-wise sum"
     '(5 8 12) ((jit ctx (list (class-of s) (class-of t)) (lambda (s t) (tensor (dimension s) k (+ (get s k) (get t k))))) s t)))
(test-equal "generate code to package an object in a list"
  '(a) ((jit ctx (list <obj>) package-return-content) 'a))
(test-equal "generate code to return the content of an RGB value"
  '(2 3 5) ((jit ctx (list <intrgb>) package-return-content) (rgb 2 3 5)))
(test-equal "build a list of values in compiled code"
  '(2 3 5) ((jit ctx (list <int> <int> <int>) build-list) 2 3 5))
(let [(i (skeleton <int>))]
  (test-equal "generate code create, define, and package return value"
    '(123)
    (address->scm ((asm ctx <long> (list <int>)
                        (apply virtual-variables (apply assemble (generate-return-code (list i)
                                                        (parameter <int>) (parameter i)))))
                   123))))
(test-eqv "get dimension of sequence"
  3 ((jit ctx (list (sequence <ubyte>)) dimension) (seq 2 3 5)))
(test-eqv "get stride of sequence"
  1 ((jit ctx (list (sequence <ubyte>)) stride) (seq 2 3 5)))
(test-eqv "number multiplied with nothing returns same number"
  5 ((jit ctx (list <int>) *) 5))
(test-equal "sequence multiplied with nothing returns same sequence"
  '(2 3 5) (to-list (* (seq 2 3 5))))
(test-eqv "determine size of integer in compiled code"
  2 ((jit ctx (list <sint>) size-of) 42))
(test-eqv "determine size of sequence (compiled)"
  6 ((jit ctx (list (sequence <sint>)) size-of) (seq <sint> 2 3 5)))
(let [(m (parameter (multiarray <int> 2)))
      (c (parameter <byte>))]
  (test-equal "shape of unary function expression is shape of argument"
    (shape m) (shape (~ m)))
  (test-equal "shape of scalar plus array expression"
    (shape m) (shape (+ c m)))
  (test-equal "shape of array plus scalar expression"
    (shape m) (shape (+ m c))))
(let [(i (parameter <int>))]
  (test-eqv "assign native integer constant to parameter"
    42 ((asm ctx <int> '() (apply virtual-variables (assemble (list (delegate i)) '() (code i 42)))))))
(test-assert "compile function returning empty list"
  (null? ((jit ctx '() (lambda () scm-eol)))))
(test-equal "call \"cons\" from compiled code"
  (cons 'a 'b) ((jit ctx (list <obj> <obj>) scm-cons) 'a 'b))
(test-equal "compile function putting object into a one-element list"
  '(a) ((jit ctx (list <obj>) (cut scm-cons <> scm-eol)) 'a))
(test-equal "compile function putting integer into a one-element list"
  '(42) ((jit ctx (list <int>) (cut scm-cons <> scm-eol)) 42))
(test-equal "compile function putting result of expression into a one-element list"
  '(170) ((jit ctx (list <int> <int>) (lambda (i j) (scm-cons (+ i j) scm-eol))) 100 70))
(test-assert "allocate memory in compiled method"
  ((jit ctx (list <ulong>) scm-gc-malloc-pointerless) 128))
(test-assert "allocate memory in compiled method"
  ((jit ctx (list <ulong>) scm-gc-malloc) 128))

(test-end "aiscm jit7")
