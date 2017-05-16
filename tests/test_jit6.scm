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
             (aiscm variable)
             (aiscm command)
             (aiscm util)
             (aiscm expression)
             (aiscm tensor)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm rgb)
             (aiscm obj)
             (aiscm pointer)
             (aiscm sequence))


(test-begin "aiscm jit6")

(define ctx (make <context>))
(define i (var <long>))
(define j (var <long>))

(let [(s (seq <int> 2 3 5))
      (t (seq <int> 3 5 7))
      (m (arr <int> (2 3 5) (7 11 13) (17 19 23)))
      (r (arr <int> (2 3 5) (7 11 13)))]
  (test-equal "switch dimensions of a 2D tensor"
    '((2 7 17) (3 11 19) (5 13 23))
    (to-list ((jit ctx (list (class-of m))
                   (lambda (m) (indexer i (indexer j (get (get m j) i) (cadr (shape m))) (car (shape m)))))
              m)))
  (test-equal "tensor macro provides local variable"
    (to-list s) (to-list ((jit ctx (list (class-of s)) (lambda (s) (dim k (get s k)))) s)))
  (test-equal "switch dimensions of a non-square 2D tensor"
    '((2 7) (3 11) (5 13))
    (to-list ((jit ctx (list (class-of r))
                   (lambda (r) (indexer i (indexer j (get (get r j) i) (cadr (shape r))) (car (shape r)))))
              r)))
  (test-equal "tensor expression for element-wise sum"
     '(5 8 12)
     (to-list ((jit ctx (list (class-of s) (class-of t)) (lambda (s t) (dim k (+ (get s k) (get t k))))) s t))))
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

(test-end "aiscm jit6")
