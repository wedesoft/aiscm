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
             (aiscm asm)
             (aiscm variable)
             (aiscm compile)
             (aiscm int))


(test-begin "aiscm compile")
(test-begin "replace-variables")
  (let [(a (var <int>))
        (x (var <sint>))
        (p (var <long>))]
  (test-equal "only put instruction into a list if there are no variables to replace"
    (list (MOV EAX 0)) (replace-variables '() (MOV EAX 0) RAX))
  (test-equal "replace input variable with allocated register"
    (list (MOV ESI ECX)) (replace-variables (list (cons a RCX)) (MOV ESI a) RAX))
  (test-equal "replace output variable with allocated register"
    (list (MOV ECX 0)) (replace-variables (list (cons a RCX)) (MOV a 0) RAX))
  (test-equal "read input variable from spill location"
    (list (MOV EDX (ptr <int> RSP 16))) (replace-variables (list (cons a (ptr <long> RSP 16))) (MOV EDX a) RAX))
  (test-equal "use temporary register for first argument and fetch value from spill location"
    (list (MOV AX (ptr <sint> RSP 16)) (CMP AX 0)) (replace-variables (list (cons x (ptr <long> RSP 16))) (CMP x 0) RAX))
  (test-equal "use correct type for temporary register"
    (list (MOV EAX (ptr <int> RSP 16)) (CMP EAX 0)) (replace-variables (list (cons a (ptr <long> RSP 16))) (CMP a 0) RAX))
  (test-equal "read and write back argument from stack into temporary register"
    (list (MOV EAX (ptr <int> RSP 16)) (ADD EAX 1) (MOV (ptr <int> RSP 16) EAX))
    (replace-variables (list (cons a (ptr <long> RSP 16))) (ADD a 1) RAX))
  (test-equal "write output value in temporary register to the stack"
    (list (MOV EAX 1) (MOV (ptr <int> RSP 16) EAX)) (replace-variables (list (cons a (ptr <long> RSP 16))) (MOV a 1) RAX))
  (test-equal "use temporary variable to implement reading from pointer to pointer"
    (list (MOV RAX (ptr <long> RSP 32)) (MOV EDX (ptr <int> RAX 8)))
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV a (ptr <int> p 8)) RAX))
  (test-equal "do not use temporary variable when reading from register pointer"
    (list (MOV EDX (ptr <int> RCX 8))) (replace-variables (list (cons a RDX) (cons p RCX)) (MOV a (ptr <int> p 8)) RAX))
  (test-equal "use temporary variable to implement writing to pointer to pointer"
    (list (MOV RAX (ptr <long> RSP 32)) (MOV (ptr <int> RAX 8) EDX))
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV (ptr <int> p 8) a) RAX)))
(test-end "replace-variables")
(test-end "aiscm compile")
