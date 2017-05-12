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
             (aiscm variable)
             (aiscm command)
             (aiscm program)
             (aiscm register-allocate)
             (aiscm asm)
             (aiscm bool)
             (aiscm int))


(test-begin "aiscm command")

(test-begin "input and output variables")
  (let [(a (var <int>))
        (b (var <int>))
        (p (var <long>))]
    (test-equal "Get input variables of MOV"
      (list b) (input (MOV a b)))
    (test-equal "Get input variables of ADD"
      (list a b) (input (ADD a b)))
    (test-equal "Prevent duplication of input variables"
      (list a) (input (ADD a a)))
    (test-equal "Get output variables of MOV"
      (list a) (output (MOV a b)))
    (test-equal "Get output variables of ADD"
      (list a) (output (ADD a b)))
    (test-equal "Get input variables of command writing to address"
      (list b a) (input (MOV (ptr <int> a) b)))
    (test-equal "Get arguments of command"
      (list a 0) (get-args (MOV a 0)))
    (test-equal "Get variables of a program"
      (list a b) (variables (list (MOV a 0) (MOV b a))))
    (test-equal "Get variables of a program using a pointer"
      (list a p) (variables (list (MOV a 0) (MOV (ptr <int> p) a))))
    (test-assert "command without any pointer arguments"
      (null? (get-ptr-args (MOV a 42))))
    (test-equal "get pointer argument of a command"
      (list p) (get-ptr-args (MOV (ptr <int> p) 42)))
    (test-assert "compiled command without variables does not have pointer arguments"
      (null? (get-ptr-args (MOV EAX 42))))
    (test-equal "only return variable arguments of pointer"
      (list p) (get-ptr-args (MOV (ptr <int> p 16) 42))))
(test-end "input and output variables")

(test-begin "copy integer values")
  (test-equal "copy signed 16-bit value"
    (MOV AX CX) (mov-signed AX CX))
  (test-equal "copy with sign extension"
    (MOVSX EAX CX) (mov-signed EAX CX))
  (test-equal "copy part of signed value"
    (MOV CL SIL) (mov-signed CL ESI))
  (test-equal "copy unsigned 16-bit value"
    (MOV AX CX) (mov-unsigned AX CX))
  (test-equal "copy with zero extension"
    (MOVZX EAX CX) (mov-unsigned EAX CX))
  (test-equal "copy part of unsigned value"
    (MOV CL SIL) (mov-unsigned CL ESI))
  (test-equal "zero-extending 32-bit value is done by default"
    (MOV EAX ECX) (mov-unsigned RAX ECX))
(test-end "copy integer values")

(test-begin "first argument of command")
  (let [(a (var <int>))
        (b (var <int>))]
  (test-equal "get first argument of ADD statement"
    a (first-argument (ADD a b)))
  (test-assert "return false if statement is compiled already"
    (not (first-argument (ADD CX DX)))))
(test-end "first argument of command")

(test-begin "register blocking")
  (test-equal "'blocked' represents the specified code segment"
    (list (MOV ECX 2) (RET)) (get-code (blocked AL (MOV ECX 2) (RET))))
  (test-equal "'blocked' stores the register to be blocked"
    RAX (get-reg (blocked RAX (MOV ECX 2) (RET))))
  (test-equal "'blocked' with empty block list has no effect"
    (list (MOV ECX 2) (RET)) (blocked '() (MOV ECX 2) (RET)))
(test-end "register blocking")

(test-begin "division and modulo")
  (test-equal "sign-extend AL, AX, EAX, and RAX"
    (list (CBW) (CWD) (CDQ) (CQO)) (map sign-extend-ax '(1 2 4 8)))
  (let [(r (var <byte>)) (a (var <byte>)) (b (var <byte>))]
    (test-equal "generate code for 8-bit signed division"
      (list (MOV AL a) (CBW) (IDIV b) (MOV r AL)) (flatten-code (filter-blocks (div r a b))))
    (test-equal "generate code for 8-bit signed remainder"
      (list (MOV AL a) (CBW) (IDIV b) (MOV AL AH) (MOV r AL)) (flatten-code (filter-blocks (mod r a b))))
    (test-eq "block RAX register when dividing"
      RAX (get-reg (div r a b))))
  (let [(r (var <ubyte>)) (a (var <ubyte>)) (b (var <ubyte>))]
    (test-equal "generate code for 8-bit unsigned division"
      (list (MOVZX AX a) (DIV b) (MOV r AL)) (flatten-code (filter-blocks (div r a b)))))
  (let [(r (var <sint>)) (a (var <sint>)) (b (var <sint>))]
    (test-equal "generate code for 16-bit signed division"
      (list (MOV AX a) (CWD) (IDIV b) (MOV r AX)) (flatten-code (filter-blocks (div r a b))))
    (test-eq "16-bit signed division blocks RDX register"
      RDX (get-reg (car (get-code (div r a b))))))
  (let [(r (var <usint>)) (a (var <usint>)) (b (var <usint>))]
      (test-equal "generate code for 16-bit unsigned division"
    (list (MOV AX a) (MOV DX 0) (DIV b) (MOV r AX)) (flatten-code (filter-blocks (div r a b))))
      (test-equal "generate code for 16-bit unsigned modulo"
    (list (MOV AX a) (MOV DX 0) (DIV b) (MOV r DX)) (flatten-code (filter-blocks (mod r a b)))))
(test-end "division and modulo")

(test-begin "shl and shr")
(let [(s (var <int>))
      (u (var <uint>))
      (n (var <byte>))]
  (test-equal "shl blocks RCX register"
    RCX (get-reg (shl s n)))
  (test-equal "shl uses SHL for unsigned input"
    (list (mov-unsigned CL n) (SHL u CL)) (filter-blocks (shl u n)))
  (test-equal "shl uses SAL for signed input"
    (list (mov-unsigned CL n) (SAL s CL)) (filter-blocks (shl s n)))
  (test-equal "shl uses SHR for unsigned input"
    (list (mov-unsigned CL n) (SHR u CL)) (filter-blocks (shr u n)))
  (test-equal "shl uses SAR for signed input"
    (list (mov-unsigned CL n) (SAR s CL)) (filter-blocks (shr s n))))
(test-end "shl and shr")

(test-begin "boolean operations")
  (let [(a (var <int>))
        (r (var <bool>))]
    (test-equal "generate code for equal zero"
      (list (TEST a a) (SETE r)) (test-zero r a))
    (test-equal "generate code for non-equal zero"
      (list (TEST a a) (SETNE r)) (test-non-zero r a)))
(test-end "boolean operations")
(test-end "aiscm command")
