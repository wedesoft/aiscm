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
(define three (make <int> #:value (const 3)))
(define p (make (pointer <byte>) #:value (const 1234)))
(define a (llvmarray p p (llvmlist three) (llvmlist one)))

(test-group "convert array to tensor"
  (test-eqv "pass-through integer"
    42 (get (expression->tensor (make <int> #:value 42)))))

(define-tensor (rebuild x) (tensor i (get x i)))
;(define-tensor (index-array n) (tensor (i n) i))
(test-group "array indexing"
  (test-equal "rebuild array"
    '(2 3 5) (to-list (rebuild (arr 2 3 5))))
  (test-skip 1)
  (test-equal "index array"
    '(0 1 2) (to-list (index-array 3))))

(test-end "aiscm tensors")
