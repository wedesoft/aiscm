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
             (srfi srfi-1)
             (srfi srfi-26)
             (rnrs bytevectors)
             (oop goops)
             (aiscm util)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm program)
             (aiscm expression)
             (aiscm register-allocate)
             (aiscm compile)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm float)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm bool)
             (aiscm rgb)
             (aiscm obj)
             (aiscm complex))


(test-begin "aiscm jit1")

(define ctx (make <context>))

(test-begin "virtual variables")
  (let [(a (var <int>))
        (b (var <int>))
        (c (var <int>))]
    (test-equal "'virtual-variables' uses the specified variables as parameters"
      (list (SUB RSP 8) (MOV ECX EDI) (ADD ECX ESI) (MOV EAX ECX) (ADD RSP 8) (RET))
      (virtual-variables (list a) (list b c) (list (MOV a b) (ADD a c) (RET))))
    (test-equal "'virtual-variables' allocates local variables"
      (list (SUB RSP 8) (MOV ECX EDI) (MOV EDX ECX) (MOV EAX EDX) (ADD RSP 8) (RET))
      (virtual-variables (list a) (list b) (list (MOV c b) (MOV a c) (RET)))))
  (let [(a (var <int>))
        (b (var <int>))]
    (test-equal "'virtual-variables' handles nested code blocks"
      (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
      (virtual-variables (list a) (list b) (list (list (MOV a b)) (RET))))
    (test-equal "'virtual-variables' maps the 7th integer parameter correctly"
      (list (SUB RSP 8) (MOV EAX (ptr <int> RSP 16)) (MOV EDX EAX) (MOV EAX EDX) (ADD RSP 8) (RET))
      (let [(args (map var (make-list 7 <int>)))] (virtual-variables (list a) args (list (MOV a (last args)) (RET))))))
  (let [(a (var <int>))]
    (test-eqv "'virtual-variables' creates separate namespaces for labels"
      3 ((asm ctx <int> '() (virtual-variables (list a) '() (list (MOV a 0) (JMP 'a) (list 'a (MOV a 2)) 'a (ADD a 3) (RET)))) )))
  (let  [(w (var <usint>))]
    (test-equal "'virtual-variables' filters out the reserved-registers information"
      (list (SUB RSP 8) (MOV AX 0) (ADD RSP 8) (RET)) (virtual-variables '() '() (list (blocked RCX (MOV w 0)) (RET))))
    (test-equal "'virtual-variables' avoids blocked registers when allocating variables"
      (list (SUB RSP 8) (MOV CX 0) (ADD RSP 8) (RET)) (virtual-variables '() '() (list (blocked RAX (MOV w 0)) (RET)))))
(test-end "virtual variables")

(let [(a (skeleton <byte>))
      (b (skeleton (pointer <byte>)))
      (c (set-pointer-offset (skeleton (pointer <int>)) 3))]
  (test-equal "element operand is value of element"
    (get a) (operand a))
  (test-equal "pointer operand is pointer to element"
    (ptr <byte> (get b)) (operand b))
  (test-equal "pointer operand can have offset"
    (ptr <int> (get c) 3) (operand c)))
(let [(out (skeleton <int>))
      (in  (skeleton <int>))]
  (test-equal "generate code for copying an integer"
    (list (list (mov-signed (get out) (get in)))) (code out in))
  (test-equal "generate code for identity function"
    (list (list (get out)) (list (get in)) (list (list (mov-signed (get out) (get in))) (RET)))
    (assemble (list out) (list in) (code out in))))
(test-equal "Use default zero-extension for 32-bit numbers"
  (list (SUB RSP 8) (MOV EAX ECX) (ADD RSP 8) (RET))
  (jit-compile (flatten-code (attach (code (skeleton <ulong>) (skeleton <uint>)) (RET)))))
(test-eqv "compile and run integer identity function"
  42 ((jit ctx (list <int>) identity) 42))
(test-eqv "compile and run boolean identity function"
  #t ((jit ctx (list <bool>) identity) #t))
(let [(out (skeleton <int>))
      (in  (skeleton (pointer <int>)))]
  (test-equal "generate code for reading integer from memory"
    (list (list (mov-signed (get out) (ptr <int> (get in))))) (code out in)))
(let [(out (skeleton (pointer <int>)))
      (in  (skeleton <int>))]
  (test-equal "generate code for writing integer to memory"
    (list (list (mov-signed (ptr <int> (get out)) (get in)))) (code out in)))
(let [(out (skeleton <int>))]
  (test-equal "Generate code for setting variable to zero"
    (list (MOV (get out) 0)) (code out 0)))
(let [(in  (skeleton (pointer <byte>)))
      (out (skeleton (pointer <byte>)))]
  (test-equal "generate code for copying a byte from one memory location to another"
    (list (SUB RSP 8) (MOV DL (ptr <byte> RAX)) (MOV (ptr <byte> RSI) DL) (ADD RSP 8) (RET))
    (jit-compile (flatten-code (attach (code out in) (RET))))))
(test-equal "compile and run identity function for array"
  '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) identity) (seq <int> 2 3 5))))
(let [(out (skeleton (multiarray <int> 2)))
      (in  (skeleton (multiarray <int> 2)))]
  (test-assert "generating code for copying a 2D array should run without error"
    (list? (code (parameter out) (parameter in)))))
(test-equal "compile and run identity function for 2D array"
  '((2 3 5) (7 9 11)) (to-list ((jit ctx (list (multiarray <int> 2)) identity) (arr <int> (2 3 5) (7 9 11)))))
(let [(out (skeleton <int>))
      (a   (skeleton <int>))
      (b   (skeleton <int>))]
  (test-equal "generate code for adding two numbers"
    (list (list (mov-signed (get out) (get a))) (ADD (get out) (get b)))
    (code (parameter out) (+ (parameter a) (parameter b)))))
(test-equal "compile and run function adding two numbers"
  42 ((jit ctx (list <int> <int>) +) 19 23))
(let [(out (skeleton <byte>))
      (in  (skeleton <int>))]
  (test-equal "generate code for copying part of integer"
    (list (SUB RSP 8) (MOV AL CL) (ADD RSP 8) (RET))
    (jit-compile (flatten-code (list (code out in) (RET))))))
(let [(out (skeleton <int>))
      (a   (skeleton <byte>))
      (b   (skeleton <usint>))]
  (test-equal "sign-extend second number when adding"
    (list (SUB RSP 8) (MOVZX ESI AX) (MOVSX ECX DL) (ADD ESI ECX) (ADD RSP 8) (RET))
    (jit-compile (flatten-code (list (code (parameter out) (+ (parameter b) (parameter a))) (RET))))))
(test-assert "create function from tensor and element"
  (+ (parameter (sequence <int>)) (parameter <int>)))
(test-assert "create function from element and tensor"
  (+ (parameter <int>) (parameter (sequence <int>))))
(test-assert "create function from two tensors"
  (+ (parameter (sequence <int>)) (parameter (sequence <int>))))
(let [(out (skeleton (sequence <int>)))
      (a   (skeleton (sequence <int>)))
      (b   (skeleton <int>))]
  (test-assert "generating code for array-scalar operation should run without error"
    (list? (code (parameter out) (+ (parameter a) (parameter b))))))
(test-equal "compile and run array-scalar operation"
  '(9 10 12) (to-list ((jit ctx (list (sequence <int>) <int>) +) (seq <int> 2 3 5) 7)))

(test-end "aiscm jit1")
