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
             (aiscm int)
             (aiscm sequence)
             (aiscm asm)
             (aiscm operation)
             (aiscm jit))


(define ctx (make <context>))

(test-begin "aiscm operation")
(test-eqv "put native constant into compiled code"
  42 ((jit ctx '() (lambda () (native-const <int> 42)))))

(test-begin "size-of in compiled code")
  (test-eqv "determine size of integer in compiled code"
    2 ((jit ctx (list <sint>) size-of) 42))
  (test-eqv "determine size of sequence (compiled)"
    6 ((jit ctx (list (sequence <sint>)) size-of) (seq <sint> 2 3 5)))
(test-end "size-of in compiled code")
(test-end "aiscm operation")
