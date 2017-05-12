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

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (test-equal "'virtual-variables' uses the specified variables as parameters"
    (list (SUB RSP 8) (MOV ECX EDI) (ADD ECX ESI) (MOV EAX ECX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b c) (list (MOV a b) (ADD a c) (RET))))
  (test-equal "'virtual-variables' allocates local variables"
    (list (SUB RSP 8) (MOV ECX EDI) (MOV EDX ECX) (MOV EAX EDX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b) (list (MOV c b) (MOV a c) (RET)))))
(test-eq "'retarget' should update target of jump statement"
  'new (get-target (retarget (JMP 'old) 'new)))
(let [(a (var <int>))
      (b (var <int>))]
  (test-equal "'pass-parameter-variables' handles nested code blocks"
    (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b) (list (list (MOV a b)) (RET))))
  (test-equal "'virtual-variables' maps the 7th integer parameter correctly"
    (list (SUB RSP 8) (MOV EAX (ptr <int> RSP 16)) (MOV EDX EAX) (MOV EAX EDX) (ADD RSP 8) (RET))
    (let [(args (map var (make-list 7 <int>)))] (virtual-variables (list a) args (list (MOV a (last args)) (RET))))))

(let [(a (var <int>))]
  (test-eqv "'virtual-variables' creates separate namespaces for labels"
    3 ((asm ctx <int> '() (virtual-variables (list a) '() (list (MOV a 0) (JMP 'a) (list 'a (MOV a 2)) 'a (ADD a 3) (RET)))) )))

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (test-equal "'repeat' loop"
      (list (SUB RSP 8) (MOV ECX 0) (MOV ESI 0) (CMP ESI EDX) (JE #x6) (INC ESI) (INC ECX) (JMP #x-a) (ADD RSP 8) (RET))
      (resolve-jumps (jit-compile (flatten-code (list (MOV a 0) (repeat 0 b (INC a)) (RET))))))
  (test-equal "'repeat' loop with offset"
    (list (SUB RSP 8) (MOV ECX 0) (MOV ESI 1) (CMP ESI EDX) (JE #x6) (INC ESI) (INC ECX) (JMP #x-a) (ADD RSP 8) (RET))
    (resolve-jumps (jit-compile (flatten-code (list (MOV a 0) (repeat 1 b (INC a)) (RET)))))))

(let [(r (var <byte>))
      (a (var <byte>))
      (b (var <byte>))]
  (test-equal "'compile' should block registers if specified"
    (list (SUB RSP 8) (MOV AL CL) (CBW) (IDIV DL) (MOV CL AL) (ADD RSP 8) (RET))
    (jit-compile (list (MOV AL a) (CBW) (IDIV b) (MOV r AL) (RET)) #:registers (list RAX RCX RDX) #:blocked (list (cons RAX '(0 . 3))))))
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (d (var <int>))
      (e (var <int>))
      (f (var <int>))
      (g (var <int>))
      (r (var <int>))]
  (test-equal "save callee-saved registers"
    (list (PUSH RBX) (SUB RSP 8) (MOV EBX 1) (ADD RSP 8) (POP RBX) (RET))
    (jit-compile (list (MOV a 1) (RET)) #:registers (list RBX RAX)))
  (test-equal "add offset for callee-saved parameters when fetching stack parameters"
    (list (PUSH RBX) (SUB RSP 8) (MOV EBX (ptr <int> RSP 24)) (MOV EBX 42) (ADD RSP 8) (POP RBX) (RET))
    (jit-compile (list (MOV g 42) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
  (test-equal "add offset for callee-saved parameters when using stack parameters"
    (list (PUSH RBX) (SUB RSP 8) (MOV EBX EAX) (MOV (ptr <int> RSP 24) EBX) (ADD RSP 8) (POP RBX) (RET))
    (jit-compile (list (MOV g r) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
  (test-equal "move parameter variable into another location if the register is blocked"
    (list (SUB RSP 8) (MOV EAX EDI) (MOV EDI EAX) (ADD RSP 8) (RET))
    (jit-compile (list (MOV EDI a) (RET))
                 #:parameters (list a)
                 #:registers (list RDI RAX RCX)
                 #:blocked (list (cons RDI '(0 . 0)))))
  (test-equal "when allocating registers preserve result variables up to RET statement"
    (list (SUB RSP 8) (MOV ECX 42) (MOV EAX 0) (MOV EAX ECX) (ADD RSP 8) (RET))
    (jit-compile (list (MOV r 42) (MOV b 0) (RET)) #:results (list r))))

(let  [(w (var <usint>))]
  (test-equal "'virtual-variables' filters out the reserved-registers information"
    (list (SUB RSP 8) (MOV AX 0) (ADD RSP 8) (RET)) (virtual-variables '() '() (list (blocked RCX (MOV w 0)) (RET))))
  (test-equal "'virtual-variables' avoids blocked registers when allocating variables"
    (list (SUB RSP 8) (MOV CX 0) (ADD RSP 8) (RET)) (virtual-variables '() '() (list (blocked RAX (MOV w 0)) (RET)))))

(test-eq "Shortcut for creating variables creates variables"
  <var> (class-of (var <int>)))
(test-eq "Shortcut for  creating variables uses specified type"
  <byte> (typecode (var <byte>)))
(test-eq "Boolean values are represented using unsigned byte"
  <ubyte> (typecode (var <bool>)))
(let  [(i (skeleton <int>))]
  (test-assert "skeleton of integer is of type integer"
    (is-a? i <int>))
  (test-assert "value of integer skeleton is a variable"
    (is-a? (value i) <var>))
  (test-eq "value of integer skeleton is of type integer"
    <int> (typecode (value i))))
(let [(s (skeleton (sequence <byte>)))]
  (test-assert "skeleton of a sequence is a sequence"
    (is-a? s (sequence <byte>)))
  (test-equal "skeleton of sequence consists of two long integer variables and an unsigned long integer"
    (list <long> <long> <ulong>) (map class-of (content (class-of s) s)))
  (test-equal "sequence skeleton is based on three variables"
    (list <var> <var> <var>) (map class-of (map get (content (class-of s) s)))))
(let [(m (skeleton (multiarray <int> 2)))]
  (test-assert "skeleton of a 2D array is a 2D array"
    (is-a? m (multiarray <int> 2)))
  (test-equal "skeleton of 2D array consists of long integer variables and an unsigned long integer"
    (list <long> <long> <long> <long> <ulong>) (map class-of (content (class-of m) m)))
  (test-equal "2D array skeleton is based on five variables"
    (make-list 5 <var>) (map class-of (map get (content (class-of m) m)))))
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
(test-eq "plus operation coerces return type correctly"
  <int> (type (+ (parameter <usint>) (parameter <byte>))))
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
