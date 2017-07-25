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
             (aiscm bool)
             (aiscm int)
             (aiscm jit))


(test-begin "aiscm bool")

(define bool-false (make <bool> #:value #f))
(define bool-true (make <bool> #:value #t))

(test-equal "equality of booleans"
  bool-false bool-false)
(test-assert "equality of booleans"
  (not (equal? bool-false bool-true)))
(test-assert "equality of booleans"
  (not (equal? bool-true bool-false)))
(test-equal "equality of booleans"
  bool-true bool-true)
(test-assert "get boolean value from bool-false"
  (not (get bool-false)))
(test-assert "get boolean value from bool-true"
  (get bool-true))
(test-assert "unequal boolean objects"
  (not (equal? bool-true bool-false)))
(test-eqv "storage size of booleans"
  1 (size-of <bool>))
(test-equal "pack 'false' value" 
  #vu8(0) (pack bool-false))
(test-equal "pack 'true' value"
  #vu8(1) (pack bool-true))
(test-eqv "querying element size of boolean"
  1 (size bool-true))
(test-assert "querying shape of boolean"
  (null? (shape bool-true)))
(test-equal "unpack 'false' value" 
  bool-false (unpack <bool> #vu8(0)))
(test-equal "unpack 'true' value" 
  bool-true (unpack <bool> #vu8(1)))
(test-equal "display boolean object" 
  "#<<bool> #f>" (call-with-output-string (lambda (port) (display bool-false port))))
(test-equal "write boolean object"
  "#<<bool> #f>" (call-with-output-string (lambda (port) (write bool-false port))))
(test-equal "type matching for #f"
  <bool> (native-type #f))
(test-equal "type matching for #t"
  <bool> (native-type #t))
(test-equal "type matching for multiple booleans"
  <bool> (native-type #f #t))
(test-assert "get value of true"
  (get bool-true))
(test-assert "get value of false"
  (not (get bool-false)))
(test-assert "set boolean to true"
  (let [(b (make <bool> #:value #f))] (set b #t) (get b)))
(test-assert "set boolean to false"
  (not (let [(b (make <bool> #:value #t))] (set b #f) (get b))))
(test-assert "return value of setting boolean to true"
  (set (make <bool> #:value #f) #t))
(test-assert "return value of setting boolean to false"
  (not (set (make <bool> #:value #t) #f)))
(test-eq "build boolean 'true'"
  #t (build <bool> '(#t)))
(test-eq "build boolean 'false'"
  #f (build <bool> '(#f)))
(test-equal "'unbuild' returns 0 for false"
  '(0) (unbuild <bool> #f))
(test-equal "'unbuild' returns 1 for true"
  '(1) (unbuild <bool> #t))
(test-equal "'&&' behaves like 'and'"
  '(#f #f #f #t) (map && '(#f #f #t #t) '(#f #t #f #t)))
(test-assert "'&&' with three arguments"
  (not (&& #t #t #f)))
(test-assert "'&&' with four arguments"
  (&& #t #t #t #t))
(test-equal "'||' behaves like 'or'"
  '(#f #t #t #t) (map || '(#f #f #t #t) '(#f #t #f #t)))
(test-assert "'||' with three arguments"
  (not (|| #f #f #f)))
(test-assert "'||' with four arguments"
  (|| #f #f #f #t))
(test-equal "'!' is like 'not'"
  '(#t #f) (map ! '(#f #t)))
(test-assert "boolean memory is pointerless"
  (pointerless? <bool>))

(test-begin "'where' using scalars")
  (test-eqv "select first scalars using where"
    2 (where #t 2 3))
  (test-eqv "select second scalar using where"
    3 (where #f 2 3))
(test-end "'where' using scalars")

(test-end "aiscm bool")
