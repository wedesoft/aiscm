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

(test-equal "adjusting the stack pointer by decreasing and increasing RSP"
  (list (SUB RSP 123) (NOP) (NOP) (NOP) (ADD RSP 123) (RET ))
  (adjust-stack-pointer 123 (list (NOP) (NOP) (NOP) (RET))))

(test-begin "parameter locations")
  (test-equal "the first parameters are register parameters"
    '(a b c) (register-parameters '(a b c)))
  (test-equal "only the first six parameters are register parameters"
    '(a b c d e f) (register-parameters '(a b c d e f g)))
  (test-assert "the first few parameters are not stored on the stack"
    (null? (stack-parameters '(a b c))))
  (test-equal "appart from the first six parameters, all parameters are stored on the stack"
    '(g h) (stack-parameters '(a b c d e f g h)))

  (let [(i (var <int>))
        (l (var <long>))]
    (test-assert "initial parameter locations for no parameters"
      (null? (register-parameter-locations '())))
    (test-equal "initial parameter location for one parameter"
      (list (cons i RDI)) (register-parameter-locations (list i)))
    (test-equal "initial parameter locations for first six parameters"
      (list RDI RSI RDX RCX R8 R9) (map cdr (register-parameter-locations (make-list 6 l))))

    (test-assert "initial stack parameter locations for no parameters"
      (null? (stack-parameter-locations '() 0)))
    (test-equal "initial parameter location of an integer stack parameter"
      (list (cons i (ptr <long> RSP 8))) (stack-parameter-locations (list i) 0))
    (test-equal "parameter locations of two stack parameters"
      (list (ptr <long> RSP 8) (ptr <long> RSP 16)) (map cdr (stack-parameter-locations (list i i) 0)))
    (test-equal "take stack offset into account when determining stack parameter locations"
      (list (ptr <long> RSP 24) (ptr <long> RSP 32)) (map cdr (stack-parameter-locations (list i i) 16)))

    (test-assert "parameter locations for empty set of parameters"
      (null? (parameter-locations '() 0)))
    (test-equal "parameter location for first parameter"
      (list (cons 'a RDI) (cons 'b RSI)) (parameter-locations '(a b) 0))
    (test-equal "parameter locations for register and stack parameters"
      (list (cons 'a RDI) (cons 'b RSI) (cons 'c RDX) (cons 'd RCX)
            (cons 'e R8) (cons 'f R9) (cons 'g (ptr <long> RSP 8)) (cons 'h (ptr <long> RSP 16)))
      (parameter-locations '(a b c d e f g h) 0))
    (test-equal "parameter locations for register and stack parameters"
      (list (cons 'a RDI) (cons 'b RSI) (cons 'c RDX) (cons 'd RCX)
            (cons 'e R8) (cons 'f R9) (cons 'g (ptr <long> RSP 24)) (cons 'h (ptr <long> RSP 32)))
      (parameter-locations '(a b c d e f g h) 16))

    (test-assert "no stack location required"
      (null? (add-stack-parameter-information '() '())))
    (test-equal "use stack location for register spilling"
      (list (cons i (ptr <int> RSP 8)))
      (add-stack-parameter-information (list (cons i #f)) (list (cons i (ptr <int> RSP 8)))))
    (test-equal "do not use stack location if register already has a location allocated"
      (list (cons i RAX))
      (add-stack-parameter-information (list (cons i RAX)) (list (cons i (ptr <int> RSP 8))))))
(test-end "parameter locations")

(test-begin "used callee saved registers")
  (test-assert "no registers in use"
    (null? (used-callee-saved '())))
  (test-equal "callee saved register in use"
    (list RBX) (used-callee-saved (list (cons 'a RBX))))
  (test-equal "remove duplicate registers"
    (list RBX) (used-callee-saved (list (cons 'a RBX) (cons 'b RBX))))
  (test-assert "ignore caller saved register"
    (null? (used-callee-saved (list (cons 'a RAX)))))
  (test-assert "ignore variables without allocated register"
    (null?  (used-callee-saved (list (cons 'a #f)))))
(test-end "used callee saved registers")
(test-end "aiscm compile")
