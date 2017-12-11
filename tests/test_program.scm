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
             (aiscm program)
             (aiscm int)
             (aiscm asm)
             (aiscm variable)
             (aiscm command))


(test-begin "aiscm program")
(test-begin "input and output of each statement")
  (let [(a (var <int>))
        (b (var <int>))
        (r (var <int>))]
    (test-equal "Get input variables of each statement"
      (list (list b) (list a b))
      (inputs (list (MOV a b) (ADD a b)) '()))
    (test-equal "Set input of return statement"
      (list (list r))
      (inputs (list (RET)) (list r)))
    (test-equal "Get output variable of each statement"
      (list (list a) (list b) '())
      (outputs (list (MOV a 42) (MOV b a) (RET)))))
(test-end "input and output of each statement")

(test-equal "'labels' should extract indices of labels"
  '((a . 1) (b . 3)) (labels (list (JMP 'a) 'a (MOV AX 0) 'b (RET))))

(test-begin "variable substitution")
  (let [(a (var <int>))
        (b (var <int>))
        (p (var <long>))]
    (test-equal "Substitute integer variable with register"
      (list (MOV ECX 42)) (substitute-variables (list (MOV a 42)) (list (cons a RCX))))
    (test-equal "Substitute variable with another"
      (MOV b 0) (substitute-variables (MOV a 0) (list (cons a b))))
    (test-equal "Substitution works with 'MOV'"
      (MOV ECX EDX) (substitute-variables (MOV a b) (list (cons a RCX) (cons b RDX))))
    (let [(p (var <long>))]
      (test-equal "Substitute long integer variable with register"
        (list (MOV RCX (ptr <long> RAX))) (substitute-variables (list (MOV p (ptr <long> RAX))) (list (cons p RCX))))
      (test-equal "Substitute pointer variable with register"
        (ptr <int> RCX) (substitute-variables (ptr <int> p) (list (cons p RCX))))
      (test-equal "Pass through pointer with empty substitution"
        (ptr <int> p 2) (substitute-variables (ptr <int> p 2) '()))
      (test-equal "Substitute pointer variable with register and offset"
        (ptr <int> RCX 5) (substitute-variables (ptr <int> p 2) (list (cons p (cons RCX 3))))))
    (let [(l (var <long>))
          (w (var <sint>))]
      (test-equal "Substitution works with 'MOVSX'"
        (MOVSX RCX EDX) (substitute-variables (MOVSX l a) (list (cons l RCX) (cons a RDX))))
      (test-equal "Substitution works with 'MOVZX'"
        (MOVZX ECX DX) (substitute-variables (MOVZX a w) (list (cons a RCX) (cons w RDX)))))
    (let [(p (var <long>))
          (q (var <long>))]
      (test-equal "Substitution works with 'LEA"
        (LEA RCX (ptr <byte> RDX)) (substitute-variables (LEA p (ptr <byte> q)) (list (cons p RCX) (cons q RDX)))))
    (test-equal "Substitution works with 'SHL"
      (SHL ECX) (substitute-variables (SHL a) (list (cons a RCX))))
    (test-equal "Substitution works with 'SHR"
      (SHR ECX) (substitute-variables (SHR a) (list (cons a RCX))))
    (test-equal "Substitution works with 'SAL"
      (SAL ECX) (substitute-variables (SAL a) (list (cons a RCX))))
    (test-equal "Substitution works with 'SAR"
      (SAR ECX) (substitute-variables (SAR a) (list (cons a RCX))))
    (test-equal "Substitution works with 'ADD'"
      (ADD ECX EDX) (substitute-variables (ADD a b) (list (cons a RCX) (cons b RDX))))
    (test-equal "Substitution works with 'PUSH'"
      (PUSH ECX) (substitute-variables (PUSH a) (list (cons a RCX))))
    (test-equal "Substitution works with 'POP'"
      (POP ECX) (substitute-variables (POP a) (list (cons a RCX))))
    (test-equal "Substitution works with 'NEG'"
      (NEG ECX) (substitute-variables (NEG a) (list (cons a RCX))))
    (test-equal "Substitution works with 'SUB'"
      (SUB ECX EDX) (substitute-variables (SUB a b) (list (cons a RCX) (cons b RDX))))
    (test-equal "Substitution works with 'IMUL'"
      (IMUL ECX EDX) (substitute-variables (IMUL a b) (list (cons a RCX) (cons b RDX))))
    (test-equal "Substitution works with 'IMUL' and three arguments"
      (IMUL ECX EDX 2) (substitute-variables (IMUL a b 2) (list (cons a RCX) (cons b RDX))))
    (test-equal "Substitution works with 'CMP'"
      (CMP ECX EDX) (substitute-variables (CMP a b) (list (cons a RCX) (cons b RDX))))
    (let [(u (var <ubyte>))
          (x (var <sint>))]
      (test-equal "Substitution works with 'SETB'"
        (SETB CL) (substitute-variables (SETB u) (list (cons u RCX))))
      (test-equal "Use correct type when substituting variable with pointer"
        (list (MOVZX DI (ptr <ubyte> RSP 24)))
        (substitute-variables (mov x u) (list (cons x RDI) (cons u (ptr <long> RSP 24)))))))
(test-end "variable substitution")

(test-equal "'relabel' should create separate namespaces for labels"
  (resolve-jumps (list (JMP 'b) (JMP 'a) 'a (NOP) 'b))
  (resolve-jumps (flatten-code (relabel (list (JMP 'a) (list (JMP 'a) 'a) (NOP) 'a)))))

(test-equal "'flatten-code' should flatten nested environments"
  (list (JMP 1) 'a (NOP) (RET)) (flatten-code (list (list (JMP 1) 'a) (NOP) (RET))))

(test-end "aiscm program")
