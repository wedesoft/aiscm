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
             (aiscm asm)
             (aiscm variable)
             (aiscm program)
             (aiscm compile)
             (aiscm int))


(test-begin "aiscm compile")
(test-begin "replace-variables")
  (let [(a (var <int>))
        (x (var <sint>))
        (p (var <long>))]
  (test-equal "put instruction into a list if there are no variables to replace"
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

(test-begin "temporary variables")
  (let [(a (var <int>))
        (b (var <int>))
        (x (var <sint>))
        (p (var <long>))]
    (test-equal "create temporary variable for first argument of instruction"
      <var> (class-of (temporary-variables (MOV a 0))))
    (test-assert "temporary variable should be distinct from first argument of instruction"
      (not (equal? a (temporary-variables (MOV a 0)))))
    (test-equal "temporary variable should have correct type"
      <sint> (typecode (temporary-variables (MOV x 0))))
    (test-assert "it should not create a temporary variable if the statement does not contain variables"
      (not (temporary-variables (MOV EAX 0))))
    (test-assert "it should not create a temporary variable if the first argument is not a variable"
      (not (temporary-variables (MOV EAX a))))
    (test-equal "create temporary variable for pointer argument to instruction"
      <var> (class-of (temporary-variables (MOV (ptr <int> p) a))))
    (test-equal "temporary variable for pointer argument needs to be long integer"
      <long> (typecode (temporary-variables (MOV (ptr <int> p) a)))))
(test-end "temporary variables")

(test-begin "unit intervals for temporary variables")
  (test-assert "create empty list of unit intervals"
    (null? (unit-intervals '())))
  (test-equal "generate unit interval for one temporary variable"
    '((a . (0 . 0))) (unit-intervals '(a)))
  (test-equal "generate unit interval for two temporary variables"
    '((a . (0 . 0)) (b . (1 . 1))) (unit-intervals '(a b)))
  (test-equal "filter out locations without temporary variable"
    '((b . (1 . 1))) (unit-intervals '(#f b)))
(test-end "unit intervals for temporary variables")

(test-begin "registers of temporary variables")
  (let [(a (var <int>))]
    (test-assert "create empty list of temporary registers"
      (null? (temporary-registers '() '())))
    (test-equal "return a temporary register"
      (list RCX) (temporary-registers (list (cons a RCX)) (list a)))
    (test-equal "return false if no temporary variable was required for a statement"
      (list #f) (temporary-registers '() (list #f))))
(test-end "registers of temporary variables")

(test-begin "generating code to move parameters to allocated location")
  (test-assert "no need to copy RSI to RAX before RDX to RCX"
    (not (need-to-copy-first (list (cons 'a RSI) (cons 'b RDX)) (list (cons 'a RAX) (cons 'b RCX)) 'a 'b)))
  (test-assert "RSI needs to be copied to RAX before copying RDX to RSI"
    (need-to-copy-first (list (cons 'a RSI) (cons 'b RDX)) (list (cons 'a RAX) (cons 'b RSI)) 'a 'b))
  (let [(a (var <int>))]
    (test-assert "no need to move variable content if source and destination location are the same"
      (null? (move-variable-content a RCX RCX)))
    (test-equal "move variable content from RDX to RCX"
      (MOV ECX EDX) (move-variable-content a RDX RCX))
    (test-assert "no need to move variable if stack locations are the same"
      (null? (move-variable-content a (ptr <long> RSP 24) (ptr <long> RSP 24))))
    (test-assert "no need to move variable if destination is undefined"
      (null? (move-variable-content a RSI #f))))
  (let [(a (var <int>))
        (b (var <int>))
        (c (var <int>))
        (d (var <int>))
        (e (var <int>))
        (f (var <int>))
        (g (var <int>))
        (r (var <int>))]
    (test-assert "no parameters to move arround"
      (null? (update-parameter-locations '() '() 0)))
    (test-equal "spill a register parameter"
      (list (MOV (ptr <int> RSP -8) EDI)) (update-parameter-locations (list a) (list (cons a (ptr <long> RSP -8))) 0))
    (test-assert "ignore parameters which are not used"
      (null? (update-parameter-locations (list a) '() 0)))
    (test-equal "load a stack parameter"
      (list (MOV EAX (ptr <int> RSP 8)))
      (update-parameter-locations (list a b c d e f g) (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 RAX)) 0))
    (test-equal "load a stack parameter taking into account the stack pointer offset"
      (list (MOV EAX (ptr <int> RSP 24)))
      (update-parameter-locations (list a b c d e f g) (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 RAX)) 16))
    (test-assert "leave parameter on stack"
      (null? (update-parameter-locations (list a b c d e f g)
                                         (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 (ptr <long> RSP 24)))
                                         16)))
    (test-equal "adjust order of copy operations to avoid overwriting parameters"
      (list (MOV EAX ESI) (MOV ESI EDI)) (update-parameter-locations (list a b) (map cons (list a b) (list RSI RAX)) 0)))
(test-end "generating code to move parameters to allocated location")

(test-begin "generate code to place result in correct register")
  (let [(r (var <int>))]
    (test-equal "return unmodified code if there is no result variable"
      (list (NOP) (RET)) (place-result-variable '() '() (list (NOP) (RET))))
    (test-equal "copy result variable into RAX if it is in another location"
      (list (NOP) (MOV EAX EDI) (RET)) (place-result-variable (list r) (list (cons r RDI)) (list (NOP) (RET))))
    (test-equal "return unmodified code if result already is residing in RAX"
      (list (NOP) (RET)) (place-result-variable (list r) (list (cons r RAX)) (list (NOP) (RET)))))
(test-end "generate code to place result in correct register")

(test-begin "put callee saved registers on stack")
  (test-equal "backup one register"
    (list (PUSH RBX) (NOP) (POP RBX) (RET)) (backup-registers (list RBX) (list (NOP) (RET))))
  (test-equal "backup two registers"
    (list (PUSH RBX) (PUSH RBP) (NOP) (POP RBP) (POP RBX) (RET)) (backup-registers (list RBX RBP) (list (NOP) (RET))))
(test-end "put callee saved registers on stack")

(test-begin "jit-compile")
  (let [(a (var <int>))
        (b (var <int>))
        (c (var <int>))
        (d (var <int>))
        (e (var <int>))
        (f (var <int>))
        (g (var <int>))
        (r (var <int>))
        (x (var <sint>))
        (p (var <long>))]
    (test-equal "Allocate a single register"
      (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET)) (jit-compile (list (MOV a 42) (RET))))
    (test-equal "Allocate a single register using custom list of registers"
      (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
    (test-equal "Allocate multiple registers"
      (list (SUB RSP 8) (MOV ECX 1) (MOV EDX 2) (ADD ECX EDX) (MOV EAX ECX) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
    (test-equal "Allocate a single register"
      (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 42) (RET))))
    (test-equal "Allocate a single register using custom list of registers"
      (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
    (test-equal "Allocate multiple registers"
      (list (SUB RSP 8) (MOV ECX 1) (MOV EDX 2) (ADD ECX EDX) (MOV EAX ECX) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
    (test-equal "Register allocation with predefined parameter register"
      (list (SUB RSP 8) (MOV EDX 1) (ADD EDX EDI) (MOV EDI EDX) (ADD RSP 8) (RET))
      (jit-compile (list (MOV b 1) (ADD b a) (MOV c b) (RET)) #:parameters (list a) #:registers (list RDI RSI RDX RCX)))
    (test-equal "Spill register parameter"
      (list (SUB RSP 16) (MOV (ptr <int> RSP 8) EDI) (MOV EDI 1) (ADD EDI (ptr <int> RSP 8)) (ADD RSP 16) (RET))
      (jit-compile (list (MOV b 1) (ADD b a) (RET)) #:parameters (list a) #:registers (list RDI RSI)))
    (test-equal "Fetch register parameter"
      (list (SUB RSP 8) (MOV EDI (ptr <int> RSP 16)) (MOV EAX EDI) (ADD RSP 8) (RET))
      (jit-compile (list (MOV r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX RDI RSI RAX)))
    (test-equal "Reuse stack location for spilled stack parameters"
      (list (SUB RSP 16)
            (MOV EAX 0)
            (MOV (ptr <int> RSP 8) EAX)
            (MOV EAX (ptr <int> RSP 8))
            (ADD EAX (ptr <int> RSP 24))
            (MOV (ptr <int> RSP 8) EAX)
            (ADD RSP 16)
            (RET))
      (jit-compile (list (MOV r 0) (ADD r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX)))
    (test-equal "Copy result to RAX register before restoring stack pointer and returning"
      (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
      (jit-compile (list (MOV r a) (RET)) #:parameters (list a) #:results (list r))))
  (let [(a (var <int>))
        (b (var <int>))]
    (test-equal "'compile' should use the specified set of registers"
      (list (SUB RSP 8) (MOV EDI 1) (MOV EAX 2) (ADD EAX 3) (ADD EDI 4) (ADD RSP 8) (RET))
      (jit-compile (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET)) #:registers (list RSI RDI RAX)))
    (test-equal "'compile' should spill variables"
      (list (SUB RSP 16)
            (MOV EAX 1)
            (MOV (ptr <int> RSP 8) EAX)
            (MOV ESI 2)
            (ADD ESI 3)
            (MOV EAX (ptr <int> RSP 8))
            (ADD EAX 4)
            (MOV (ptr <int> RSP 8) EAX)
            (ADD RSP 16)
            (RET))
            (jit-compile (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                         #:registers (list RAX RSI)))
    (let  [(c (var <int>))]
      (test-equal "'compile' should assign separate stack locations"
        (list (SUB RSP 24)
              (MOV ESI 1)
              (MOV (ptr <int> RSP 8) ESI)
              (MOV ESI 2)
              (MOV (ptr <int> RSP 16) ESI)
              (MOV EAX 3)
              (ADD EAX 4)
              (MOV ESI (ptr <int> RSP 16))
              (ADD ESI 5)
              (MOV (ptr <int> RSP 16) ESI)
              (MOV ESI (ptr <int> RSP 8))
              (ADD ESI 6)
              (MOV (ptr <int> RSP 8) ESI)
              (ADD RSP 24)
              (RET))
              (jit-compile (list (MOV a 1) (MOV b 2) (MOV c 3) (ADD c 4) (ADD b 5) (ADD a 6) (RET))
                           #:registers (list RSI RAX))))
    (test-equal "'compile' should save callee-saved registers"
      (list (PUSH RBX)
            (SUB RSP 16)
            (MOV EBX 1)
            (MOV (ptr <int> RSP 8) EBX)
            (MOV EAX 2)
            (ADD EAX 3)
            (MOV EBX (ptr <int> RSP 8))
            (ADD EBX 4)
            (MOV (ptr <int> RSP 8) EBX)
            (ADD RSP 16)
            (POP RBX)
            (RET))
            (jit-compile (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET)) #:registers (list RBX RAX))))

  (let [(r (var <byte>))
        (a (var <byte>))
        (b (var <byte>))]
    (test-equal "'compile' should block registers if specified"
      (list (SUB RSP 8) (MOV AL CL) (CBW) (IDIV DL) (MOV CL AL) (ADD RSP 8) (RET))
      (jit-compile (list (MOV AL a) (CBW) (IDIV b) (MOV r AL) (RET))
                   #:registers (list RAX RCX RDX)
                   #:blocked (list (cons RAX '(0 . 3))))))

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
(test-end "jit-compile")
(test-end "aiscm compile")
