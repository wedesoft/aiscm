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
      (MOVZX DI (ptr <ubyte> RSP 24))
      (substitute-variables (mov-unsigned x u) (list (cons x RDI) (cons u (ptr <long> RSP 24)))))))

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
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (x (var <sint>))]
  (test-assert "no variables means no unallocated variables"
    (null? (unallocated-variables '())))
  (test-equal "return the unallocated variable"
    (list a) (unallocated-variables (list (cons a #f))))
  (test-assert "ignore the variable with register allocated"
    (null? (unallocated-variables (list (cons a RAX)))))
  (test-assert "no variables means no variables with register allocated"
    (null? (register-allocations '())))
  (test-equal "return the variable with register allocation information"
    (list (cons a RAX)) (register-allocations (list (cons a RAX))))
  (test-assert "filter out the variable which does not have a register allocated"
    (null? (register-allocations (list (cons a #f)))))
  (test-assert "assigning spill locations to an empty list of variables returns an empty list"
    (null?  (assign-spill-locations '() 16 8)))
  (test-equal "assign spill location to a variable"
    (list (cons a (ptr <long> RSP 16)))  (assign-spill-locations (list a) 16 8))
  (test-equal "assign spill location with a different offset"
    (list (cons a (ptr <long> RSP 32)))  (assign-spill-locations (list a) 32 8))
  (test-equal "use increasing offsets for spill locations"
    (list (cons a (ptr <long> RSP 16)) (cons b (ptr <long> RSP 24))) (assign-spill-locations (list a b) 16 8))
  (test-assert "do nothing if there are no variables"
    (null? (add-spill-information '() 16 8)))
  (test-equal "pass through variables with register allocation information"
    (list (cons a RAX)) (add-spill-information (list (cons a RAX)) 16 8))
  (test-equal "allocate spill location for a variable"
    (list (cons a (ptr <long> RSP 16))) (add-spill-information (list (cons a #f)) 16 8)))

(test-assert "no predefined variables"
  (null? (blocked-predefined '() '() '())))
(test-equal "detect predefined variable with blocked register"
  (list (cons 'a RDI)) (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(1 . 3)))))
(test-assert "ignore predefined variables if no registers are blocked"
  (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) '())))
(test-assert "ignore predefined variables if the associated register is not blocked"
  (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(3 . 4))))))
(test-assert "ignore unused variable when checking for blocked registers"
  (null? (blocked-predefined (list (cons 'a RDI)) '() (list (cons RDI '(2 . 3))))))
(test-assert "only consider register associated with variable when blocking"
  (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RAX '(1 . 3))))))

(test-assert "no predefined variables with blocked registers to move"
  (null? (move-blocked-predefined '())))
(let [(a (var <int>))]
  (test-equal "copy variable from blocked register"
    (list (MOV a RAX)) (move-blocked-predefined (list (cons a RAX)))))

(test-equal "no predefinitions to discard"
  (list (cons 'a RDI)) (non-blocked-predefined (list (cons 'a RDI)) '()))
(test-equal "discard predefined variables which are blocked"
  '() (non-blocked-predefined (list (cons 'a RDI)) (list (cons 'a RDI))))
(test-equal "only discard predefined variables which are blocked"
  (list (cons 'b RSI)) (non-blocked-predefined (list (cons 'a RDI) (cons 'b RSI)) (list (cons 'a RDI))))

(test-assert "linear scan with no variables returns empty mapping"
  (null? (linear-scan-coloring '() '() '() '())))
(test-equal "allocate single variable"
  (list (cons 'a RAX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) '() '()))
(test-equal "reuse register with two variables"
  (list (cons 'a RAX) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 0)) (b . (1 . 1))) (list RAX RCX) '() '()))
(test-equal "allocate different registers for two variables conflicting at index 1"
  (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) '() '()))
(test-equal "sort live intervals by beginning of interval before performing linear-scan register allocation"
  (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((b . (1 . 1)) (a . (0 . 1))) (list RAX RCX) '() '()))
(test-equal "allocate different registers for two variables conflicting at index 0"
  (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 0)) (b . (0 . 1))) (list RAX RCX) '() '()))
(test-equal "mark last variable for spilling if it has a longer live interval"
  (list (cons 'a RAX) (cons 'b #f)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 3))) (list RAX) '() '()))
(test-equal "mark first variable for spilling if it has a longer live interval"
  (list (cons 'a #f) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 3)) (b . (1 . 1))) (list RAX) '() '()))
(test-equal "do not spill same variable twice"
  (list (cons 'a #f) (cons 'b #f) (cons 'c RAX))
  (linear-scan-coloring '((a . (0 . 5)) (b . (1 . 4)) (c . (2 . 3))) (list RAX) '() '()))
(test-equal "use predefined register for variable"
  (list (cons 'a RCX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) (list (cons 'a RCX)) '()))
(test-equal "predefined registers take priority over normal register allocations"
  (list (cons 'a RCX) (cons 'b RAX))
  (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) (list (cons 'a RCX)) '()))
(let [(a (var <int>))]
  (test-equal "do not allocate register if it is blocked while the variable is live"
    (list (cons a RCX))
    (linear-scan-coloring (list (cons a '(0 . 1))) (list RAX RCX) '() (list (cons RAX '(1 . 2))))))

(test-assert "no need to copy RSI to RAX before RDX to RCX"
  (not (need-to-copy-first (list (cons 'a RSI) (cons 'b RDX)) (list (cons 'a RAX) (cons 'b RCX)) 'a 'b)))
(test-assert "RSI needs to be copied to RAX before copying RDX to RSI"
  (need-to-copy-first (list (cons 'a RSI) (cons 'b RDX)) (list (cons 'a RAX) (cons 'b RSI)) 'a 'b))

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
      (r (var <int>))
      (x (var <sint>))
      (p (var <long>))]
  (test-eqv "count zero spilled variables"
    0 (number-spilled-variables '() '()))
  (test-eqv "count one spilled variable"
    1 (number-spilled-variables '((a . #f)) '()))
  (test-eqv "ignore allocated variables when counting spilled variables"
    0 (number-spilled-variables (list (cons a RAX)) '()))
  (test-eqv "do not count stack parameters when allocating stack space"
    0 (number-spilled-variables '((a . #f)) '(a)))
  (test-eqv "allocate stack space if spilled variable is not a stack parameter"
    1 (number-spilled-variables '((a . #f)) '(b)))

  (test-assert "an empty program needs no temporary variables"
    (null? (temporary-variables '())))
  (test-equal "create temporary variable for first argument of instruction"
    (list <var>) (map class-of (temporary-variables (list (MOV a 0)))))
  (test-assert "temporary variable should be distinct from first argument of instruction"
    (not (equal? (list a) (temporary-variables (list (MOV a 0))))))
  (test-equal "temporary variable should have correct type"
    (list <sint>) (map typecode (temporary-variables (list (MOV x 0)))))
  (test-equal "it should not create a temporary variable if the statement does not contain variables"
    (list #f) (temporary-variables (list (MOV EAX 0))))
  (test-equal "it should not create a temporary variable if the first argument is not a variable"
    (list #f) (temporary-variables (list (MOV EAX a))))
  (test-equal "create temporary variable for pointer argument to instruction"
    (list <var>) (map class-of (temporary-variables (list (MOV (ptr <int> p) a)))))
  (test-equal "temporary variable for pointer argument needs to be long integer"
    (list <long>) (map typecode (temporary-variables (list (MOV (ptr <int> p) a)))))

  (test-assert "create empty list of unit intervals"
    (null? (unit-intervals '())))
  (test-equal "generate unit interval for one temporary variable"
    '((a . (0 . 0))) (unit-intervals '(a)))
  (test-equal "generate unit interval for two temporary variables"
    '((a . (0 . 0)) (b . (1 . 1))) (unit-intervals '(a b)))
  (test-equal "filter out locations without temporary variable"
    '((b . (1 . 1))) (unit-intervals '(#f b)))
  (test-assert "create empty list of temporary registers"
    (null? (temporary-registers '() '())))
  (test-equal "return a temporary register"
    (list RCX) (temporary-registers (list (cons a RCX)) (list a)))
  (test-equal "return false if no temporary variable was required for a statement"
    (list #f) (temporary-registers '() (list #f)))

  (test-equal "Allocate a single register"
    (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET)) (linear-scan-allocate (list (MOV a 42) (RET))))
  (test-equal "Allocate a single register using custom list of registers"
    (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
  (test-equal "Allocate multiple registers"
    (list (SUB RSP 8) (MOV ECX 1) (MOV EDX 2) (ADD ECX EDX) (MOV EAX ECX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
  (test-equal "Allocate a single register"
    (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 42) (RET))))
  (test-equal "Allocate a single register using custom list of registers"
    (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
  (test-equal "Allocate multiple registers"
    (list (SUB RSP 8) (MOV ECX 1) (MOV EDX 2) (ADD ECX EDX) (MOV EAX ECX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
  (test-equal "Register allocation with predefined parameter register"
    (list (SUB RSP 8) (MOV EDX 1) (ADD EDX EDI) (MOV EDI EDX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV b 1) (ADD b a) (MOV c b) (RET)) #:parameters (list a) #:registers (list RDI RSI RDX RCX)))
  (test-equal "Spill register parameter"
    (list (SUB RSP 16) (MOV (ptr <int> RSP 8) EDI) (MOV EDI 1) (ADD EDI (ptr <int> RSP 8)) (ADD RSP 16) (RET))
    (linear-scan-allocate (list (MOV b 1) (ADD b a) (RET)) #:parameters (list a) #:registers (list RDI RSI)))
  (test-equal "Fetch register parameter"
    (list (SUB RSP 8) (MOV EDI (ptr <int> RSP 16)) (MOV EAX EDI) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX RDI RSI RAX)))
  (test-equal "Reuse stack location for spilled stack parameters"
    (list (SUB RSP 16)
          (MOV EAX 0)
          (MOV (ptr <int> RSP 8) EAX)
          (MOV EAX (ptr <int> RSP 8))
          (ADD EAX (ptr <int> RSP 24))
          (MOV (ptr <int> RSP 8) EAX)
          (ADD RSP 16)
          (RET))
    (linear-scan-allocate (list (MOV r 0) (ADD r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX)))
  (test-equal "Copy result to RAX register before restoring stack pointer and returning"
    (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV r a) (RET)) #:parameters (list a) #:results (list r)))

  (test-equal "get first argument of ADD statement"
    a (first-argument (ADD a b)))
  (test-assert "return false if statement is compiled already"
    (not (first-argument (ADD CX DX))))

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
    (replace-variables (list (cons a RDX) (cons p (ptr <long> RSP 32))) (MOV (ptr <int> p 8) a) RAX))

  (test-equal "'virtual-variables' uses the specified variables as parameters"
    (list (SUB RSP 8) (MOV ECX EDI) (ADD ECX ESI) (MOV EAX ECX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b c) (list (MOV a b) (ADD a c) (RET))))
  (test-equal "'virtual-variables' allocates local variables"
    (list (SUB RSP 8) (MOV ECX EDI) (MOV EDX ECX) (MOV EAX EDX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b) (list (MOV c b) (MOV a c) (RET)))))
(test-eq "'retarget' should update target of jump statement"
  'new (get-target (retarget (JMP 'old) 'new)))
(test-equal "'flatten-code' should flatten nested environments"
  (list (JMP 1) 'a (NOP) (RET)) (flatten-code (list (list (JMP 1) 'a) (NOP) (RET))))
(let [(a (var <int>))
      (b (var <int>))]
  (test-equal "'pass-parameter-variables' handles nested code blocks"
    (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
    (virtual-variables (list a) (list b) (list (list (MOV a b)) (RET))))
  (test-equal "'virtual-variables' maps the 7th integer parameter correctly"
    (list (SUB RSP 8) (MOV EAX (ptr <int> RSP 16)) (MOV EDX EAX) (MOV EAX EDX) (ADD RSP 8) (RET))
    (let [(args (map var (make-list 7 <int>)))] (virtual-variables (list a) args (list (MOV a (last args)) (RET))))))
(test-equal "'relabel' should create separate namespaces for labels"
  (resolve-jumps (list (JMP 'b) (JMP 'a) 'a (NOP) 'b))
  (resolve-jumps (flatten-code (relabel (list (JMP 'a) (list (JMP 'a) 'a) (NOP) 'a)))))

(let [(a (var <int>))]
  (test-eqv "'virtual-variables' creates separate namespaces for labels"
    3 ((asm ctx <int> '() (virtual-variables (list a) '() (list (MOV a 0) (JMP 'a) (list 'a (MOV a 2)) 'a (ADD a 3) (RET)))) )))

(let [(r (var <int>))]
  (test-equal "return unmodified code if there is no result variable"
    (list (NOP) (RET)) (place-result-variable '() '() (list (NOP) (RET))))
  (test-equal "copy result variable into RAX if it is in another location"
    (list (NOP) (MOV EAX EDI) (RET)) (place-result-variable (list r) (list (cons r RDI)) (list (NOP) (RET))))
  (test-equal "return unmodified code if result already is residing in RAX"
    (list (NOP) (RET)) (place-result-variable (list r) (list (cons r RAX)) (list (NOP) (RET)))))

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

(test-equal "backup one register"
  (list (PUSH RBX) (NOP) (POP RBX) (RET)) (backup-registers (list RBX) (list (NOP) (RET))))
(test-equal "backup two registers"
  (list (PUSH RBX) (PUSH RBP) (NOP) (POP RBP) (POP RBX) (RET)) (backup-registers (list RBX RBP) (list (NOP) (RET))))
(let [(a (var <int>))
      (b (var <int>))]
  (test-equal "'linear-scan-allocate' should use the specified set of registers"
    (list (SUB RSP 8) (MOV EDI 1) (MOV EAX 2) (ADD EAX 3) (ADD EDI 4) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET)) #:registers (list RSI RDI RAX)))
  (test-equal "'linear-scan-allocate' should spill variables"
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
          (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                #:registers (list RAX RSI)))
  (let  [(c (var <int>))]
    (test-equal "'linear-scan-allocate' should assign separate stack locations"
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
            (linear-scan-allocate (list (MOV a 1) (MOV b 2) (MOV c 3) (ADD c 4) (ADD b 5) (ADD a 6) (RET))
                                  #:registers (list RSI RAX))))
  (test-equal "'linear-scan-allocate' should save callee-saved registers"
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
          (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                #:registers (list RBX RAX))))
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (test-equal "Spill register-parameter to the stack"
    (list (SUB RSP 16)
          (MOV (ptr <int> RSP 8) ESI)
          (MOV ESI EDI)
          (ADD ESI (ptr <int> RSP 8))
          (ADD RSP 16)
          (RET))
          (virtual-variables '() (list a b) (list (MOV c a) (ADD c b) (RET)) #:registers (list RSI RDI RAX)))
  (test-equal "'repeat' loop"
      (list (SUB RSP 8) (MOV ECX 0) (MOV ESI 0) (CMP ESI EDX) (JE #x6) (INC ESI) (INC ECX) (JMP #x-a) (ADD RSP 8) (RET))
      (resolve-jumps (linear-scan-allocate (flatten-code (list (MOV a 0) (repeat 0 b (INC a)) (RET))))))
  (test-equal "'repeat' loop with offset"
    (list (SUB RSP 8) (MOV ECX 0) (MOV ESI 1) (CMP ESI EDX) (JE #x6) (INC ESI) (INC ECX) (JMP #x-a) (ADD RSP 8) (RET))
    (resolve-jumps (linear-scan-allocate (flatten-code (list (MOV a 0) (repeat 1 b (INC a)) (RET)))))))
(test-equal "'blocked' represents the specified code segment"
  (list (MOV ECX 2) (RET)) (get-code (blocked AL (MOV ECX 2) (RET))))
(test-equal "'blocked' stores the register to be blocked"
  RAX (get-reg (blocked RAX (MOV ECX 2) (RET))))
(test-equal "'blocked' with empty block list has no effect"
  (list (MOV ECX 2) (RET)) (blocked '() (MOV ECX 2) (RET)))
(test-equal "'filter-blocks' should remove blocked-register information"
  (list (MOV ECX 2) (RET)) (filter-blocks (blocked RAX (MOV ECX 2) (RET))))
(test-equal "'filter-blocks' should work recursively"
  (list (MOV EDX 2) 'x (list (RET))) (filter-blocks (blocked RDX (MOV EDX 2) 'x (blocked RAX (RET)))))
(test-equal "'blocked-intervals' should extract the blocked intervals for each register"
  (list (cons RAX '(0 . 1))) (blocked-intervals (blocked RAX (MOV EAX 0) (RET))))
(test-equal "Blocked intervals within a program should be offset correctly"
  (list (cons RAX '(1 . 1))) (blocked-intervals (list (MOV EAX 0) (blocked RAX (RET)))))
(test-equal "The offsets of 'blocked-intervals' should refer to the flattened code"
  (list (cons RAX '(2 . 2))) (blocked-intervals (list (list (MOV EAX 0) (NOP)) (blocked RAX (RET)))))
(test-equal "'blocked-intervals' should work recursively"
  (list (cons RAX '(1 . 4)) (cons RDX '(2 . 3)))
  (blocked-intervals (list 'x (blocked RAX (MOV AX 0) (blocked RDX (MOV DX 0) (IDIV CX)) (RET)))))
(test-equal "'blocked' with list of registers blocks all of them"
  (list (cons RCX '(0 . 1)) (cons RDX '(0 . 1))) (blocked-intervals (blocked (list RCX RDX) (MOV ECX 2) (RET))))
(let [(r (var <byte>))
      (a (var <byte>))
      (b (var <byte>))]
  (test-equal "'linear-scan-allocate' should block registers if specified"
    (list (SUB RSP 8) (MOV AL CL) (CBW) (IDIV DL) (MOV CL AL) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV AL a) (CBW) (IDIV b) (MOV r AL) (RET)) #:registers (list RAX RCX RDX) #:blocked (list (cons RAX '(0 . 3))))))
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
    (linear-scan-allocate (list (MOV a 1) (RET)) #:registers (list RBX RAX)))
  (test-equal "add offset for callee-saved parameters when fetching stack parameters"
    (list (PUSH RBX) (SUB RSP 8) (MOV EBX (ptr <int> RSP 24)) (MOV EBX 42) (ADD RSP 8) (POP RBX) (RET))
    (linear-scan-allocate (list (MOV g 42) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
  (test-equal "add offset for callee-saved parameters when using stack parameters"
    (list (PUSH RBX) (SUB RSP 8) (MOV EBX EAX) (MOV (ptr <int> RSP 24) EBX) (ADD RSP 8) (POP RBX) (RET))
    (linear-scan-allocate (list (MOV g r) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
  (test-equal "move parameter variable into another location if the register is blocked"
    (list (SUB RSP 8) (MOV EAX EDI) (MOV EDI EAX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV EDI a) (RET))
                          #:parameters (list a)
                          #:registers (list RDI RAX RCX)
                          #:blocked (list (cons RDI '(0 . 0)))))
  (test-equal "when allocating registers preserve result variables up to RET statement"
    (list (SUB RSP 8) (MOV ECX 42) (MOV EAX 0) (MOV EAX ECX) (ADD RSP 8) (RET))
    (linear-scan-allocate (list (MOV r 42) (MOV b 0) (RET)) #:results (list r))))

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
  (linear-scan-allocate (flatten-code (attach (code (skeleton <ulong>) (skeleton <uint>)) (RET)))))
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
    (linear-scan-allocate (flatten-code (attach (code out in) (RET))))))
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
    (linear-scan-allocate (flatten-code (list (code out in) (RET))))))
(test-eq "plus operation coerces return type correctly"
  <int> (type (+ (parameter <usint>) (parameter <byte>))))
(let [(out (skeleton <int>))
      (a   (skeleton <byte>))
      (b   (skeleton <usint>))]
  (test-equal "sign-extend second number when adding"
    (list (SUB RSP 8) (MOVZX ESI AX) (MOVSX ECX DL) (ADD ESI ECX) (ADD RSP 8) (RET))
    (linear-scan-allocate (flatten-code (list (code (parameter out) (+ (parameter b) (parameter a))) (RET))))))
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
