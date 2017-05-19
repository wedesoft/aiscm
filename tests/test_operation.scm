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
             (oop goops)
             (aiscm bool)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm asm)
             (aiscm command)
             (aiscm program)
             (aiscm expression)
             (aiscm operation)
             (aiscm compile)
             (aiscm jit)
             (aiscm util))


(define ctx (make <context>))

(test-begin "aiscm operation")

;(test-skip 43)
(test-eqv "put native constant into compiled code"
  42 ((jit ctx '() (lambda () (native-const <int> 42)))))

(test-begin "size-of in compiled code")
  (test-eqv "determine size of integer in compiled code"
    2 ((jit ctx (list <sint>) size-of) 42))
  (test-eqv "determine size of sequence (compiled)"
    6 ((jit ctx (list (sequence <sint>)) size-of) (seq <sint> 2 3 5)))
(test-end "size-of in compiled code")

(test-equal "Create function object using mutating machine instruction"
  -42 ((jit ctx (list <int>) (lambda (x) (make-function 'name identity (mutating-code NEG) (list x)))) 42))

(test-begin "value needs conversion")
  (let* [(a (parameter <sint>))
         (f (~ a))]
    (test-assert "no conversion required if types are the same"
      (not (need-conversion? <sint> <sint>)))
    (test-assert "conversion required if types are different"
      (need-conversion? <int> <sint>))
    (test-assert "no conversion required if integers have the same size"
      (not (need-conversion? <byte> <ubyte>)))
    (test-assert "no conversion required when changing bytes to booleans"
      (not (need-conversion? <byte> <bool>)))
    (test-assert "no conversion required when changing boolean to bytes"
      (not (need-conversion? <bool> <ubyte>))))
(test-end "value needs conversion")

(test-begin "copying values")
  (let [(out (skeleton <int>))
        (in  (skeleton <int>))]
    (test-equal "generate code for copying an integer"
      (list (list (mov-signed (get out) (get in)))) (code out in))
    (test-equal "generate code for identity function"
      (list (list (get out)) (list (get in)) (list (list (mov-signed (get out) (get in))) (RET)))
      (assemble (list out) (list in) (code out in))))
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
  (let [(out (skeleton (multiarray <int> 2)))
        (in  (skeleton (multiarray <int> 2)))]
    (test-assert "generating code for copying a 2D array should run without error"
      (list? (code (parameter out) (parameter in)))))
  (let [(out (skeleton <byte>))
        (in  (skeleton <int>))]
    (test-equal "generate code for copying part of integer"
      (list (SUB RSP 8) (MOV AL CL) (ADD RSP 8) (RET))
      (jit-compile (flatten-code (list (code out in) (RET))))))
  (let [(i (parameter <int>))]
    (test-eqv "assign native integer constant to parameter"
      42 ((asm ctx <int> '() (apply virtual-variables (assemble (list (delegate i)) '() (code i 42)))))))
(test-end "copying values")

(test-begin "insert intermediate value")
  (let* [(a    (skeleton <sint>))
         (expr (let-skeleton [(tmp <sint> a)] tmp))
         (tmp  (last expr))]
    (test-equal "Use intermediate value"
      (list (code tmp a) tmp) expr)
    (test-equal "Add more code"
      (NOP) (last (let-skeleton [(tmp <sint> a)] (NOP) (NOP))))
    (test-assert "Use intermediate parameter"
      (is-a? (last (let-parameter [(tmp <sint> (parameter a))] tmp)) <param>)))
(test-end "insert intermediate value")

(test-begin "check whether intermediate value is required")
  (let* [(a (parameter <sint>))
         (f (~ a))]
    (test-assert "Compilation of function always requires intermediate value"
      (code-needs-intermediate? <sint> f))
    (test-assert "Value does not require intermediate value"
      (not (code-needs-intermediate? <sint> a)))
    (test-assert "Value of different size requires intermediate value"
      (code-needs-intermediate? <int> a)))
(test-end "check whether intermediate value is required")

(test-begin "determine operand for machine instruction")
  (let [(a (skeleton <byte>))
        (b (skeleton (pointer <byte>)))
        (c (set-pointer-offset (skeleton (pointer <int>)) 3))]
    (test-equal "element operand is value of element"
      (get a) (operand a))
    (test-equal "pointer operand is pointer to element"
      (ptr <byte> (get b)) (operand b))
    (test-equal "pointer operand can have offset"
      (ptr <int> (get c) 3) (operand c)))
(test-end "determine operand for machine instruction")

(test-begin "force intermediate values where required")
  (let [(a   (parameter <sint>))]
    (test-equal "Pass through parameters to specified function by default"
      (list a) (force-parameters (list <sint>) (list a) code-needs-intermediate? identity))
    (let* [(forced       (force-parameters (list <int>) (list a) code-needs-intermediate? identity))
           (intermediate (last forced))]
      (test-equal "Create parameter of target type if type is different"
        <int> (type intermediate))
      (test-equal "Create preamble for initialising intermediate values"
        (attach (code intermediate a) intermediate) forced)
      (test-equal "Alternatively force all parameters to the same type"
        <int> (type (last (force-parameters <int> (list a) code-needs-intermediate? identity))))))
(test-end "force intermediate values where required")

(test-equal "Use default zero-extension for 32-bit numbers"
  (list (SUB RSP 8) (MOV EAX ECX) (ADD RSP 8) (RET))
  (jit-compile (flatten-code (attach (code (skeleton <ulong>) (skeleton <uint>)) (RET)))))

(test-begin "identity function")
  (test-eqv "compile and run integer identity function"
    42 ((jit ctx (list <int>) identity) 42))
  (test-eqv "compile and run boolean identity function"
    #t ((jit ctx (list <bool>) identity) #t))
  (test-equal "compile and run identity function for array"
    '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) identity) (seq <int> 2 3 5))))
  (test-equal "compile and run identity function for 2D array"
    '((2 3 5) (7 9 11)) (to-list ((jit ctx (list (multiarray <int> 2)) identity) (arr <int> (2 3 5) (7 9 11)))))
(test-end "identity function")

(test-begin "unary +")
  (test-equal "plus passes through values"
    42 ((jit ctx (list <int>) +) 42))
  (test-equal "Compiling a plus operation with different types creates an equivalent machine program"
    3 ((jit ctx (list <int> <sint> <ubyte>) +) 2 -3 4))
(test-end "unary +")

(test-begin "unary -")
  (let [(out (skeleton <int>))
        (a   (skeleton <int>))]
    (test-equal "generate code for negating number"
      (list (list (mov-signed (get out) (get a))) (NEG (get out))) (code (parameter out) (- (parameter a)))))
  (test-equal "Negate integer"
    -42 ((jit ctx (list <int>) -) 42))
  (test-equal "compile and run function for negating array"
    '(-2 3 -5) (to-list ((jit ctx (list (sequence <int>)) -) (seq <int> 2 -3 5))))
  (test-equal "negate integer twice"
    42 ((jit ctx (list <int>) (compose - -)) 42))
(test-end "unary -")

(test-equal "unary ~"
  '(253 252 250) (to-list ((jit ctx (list (sequence <ubyte>)) ~) (seq 2 3 5))))

(test-begin "binary +")
  (let [(out (skeleton <int>))
        (a   (skeleton <int>))
        (b   (skeleton <int>))]
    (test-equal "generate code for adding two numbers"
      (list (list (mov-signed (get out) (get a))) (ADD (get out) (get b)))
      (code (parameter out) (+ (parameter a) (parameter b)))))
  (test-equal "compile and run function adding two numbers"
    42 ((jit ctx (list <int> <int>) +) 19 23))
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
  (test-equal "compile and run scalar-array operation"
    '(9 10 12) (to-list ((jit ctx (list <int> (sequence <int>)) +) 7 (seq <int> 2 3 5))))
  (test-equal "sign-extend second number when adding value from pointer"
    '(9 10 12) (to-list ((jit ctx (list <int> (sequence <byte>)) +) 7 (seq <byte> 2 3 5))))
  (test-equal "compile and run array-array operation"
    '(9 14 18) (to-list ((jit ctx (list (sequence <int>) (sequence <int>)) +) (seq <int> 2 3 5) (seq <int> 7 11 13))))
  (test-equal "compile and run 2D array-scalar operation"
    '((7) (8)) (to-list ((jit ctx (list (multiarray <int> 2) <int>) +) (arr <int> (2) (3)) 5)))
  (test-equal "compile and run 2D scale-array operation"
    '((7) (8)) (to-list ((jit ctx (list <int> (multiarray <int> 2)) +) 5 (arr <int> (2) (3)))))
  (test-equal "compile and run operation involving 1D and 2D array"
    '((0 1 3) (0 2 4))
    (to-list ((jit ctx (list (sequence <byte>) (multiarray <ubyte> 2)) +) (seq -2 -3) (arr (2 3 5) (3 5 7)))))
  (let [(a (parameter <int>))
        (b (parameter <int>))
        (p (parameter (pointer <int>)))]
    (test-equal "generate code to increment add a number"
      (ADD (value a) (value b)) (+= a b))
    (test-equal "generate code to increment add a number stored in memory"
      (ADD (value a) (ptr <int> (value p))) (+= a p)))
(test-end "binary +")

;(test-skip 20)
(test-begin "binary -")
  (test-equal "subtract byte fro integer sequence"
    '(0 1 2) (to-list ((jit ctx (list (sequence <int>) <byte>) -) (seq <int> 1 2 3) 1)))
(test-end "binary -")

(test-equal "binary *"
  '(2 4 6) (to-list ((jit ctx (list (sequence <int>) <int>) *) (seq <int> 1 2 3) 2)))

(test-begin "binary min")
  (test-equal "get minor number of two integers (first case)"
    2 ((jit ctx (list <usint> <usint>) min) 2 3))
  (test-equal "get minor number of two integers (second case)"
    2 ((jit ctx (list <usint> <usint>) min) 3 2))
  (test-equal "get minor number of two unsigned integers (first case)"
    32767 ((jit ctx (list <usint> <usint>) min) 32767 32768))
  (test-equal "get minor number of two unsigned integers (second case)"
    32767 ((jit ctx (list <usint> <usint>) min) 32768 32767))
  (test-equal "get minor number of two signed integers"
    -1 ((jit ctx (list <sint> <sint>) min) -1 1))
  (test-equal "get minor number of two unsigned bytes (first case)"
    2 ((jit ctx (list <ubyte> <ubyte>) min) 2 3))
  (test-equal "get minor number of two unsigned bytes (second case)"
    2 ((jit ctx (list <ubyte> <ubyte>) min) 3 2))
  (test-equal "get minor number of two bytes (first case)"
    -1 ((jit ctx (list <byte> <byte>) min) -1 1))
  (test-equal "get minor number of two bytes (second case)"
    -1 ((jit ctx (list <byte> <byte>) min) 1 -1))
(test-end "binary min")

(test-begin "binary max")
  (test-equal "get major number of two unsigned integers (first case)"
    32768 ((jit ctx (list <usint> <usint>) max) 32767 32768))
  (test-equal "get major number of two unsigned integers (second case)"
    32768 ((jit ctx (list <usint> <usint>) max) 32768 32767))
  (test-equal "get major number of two signed integers"
    1 ((jit ctx (list <sint> <sint>) max) -1 1))
  (test-equal "get major number of signed and unsigned short integers"
    32768 ((jit ctx (list <sint> <usint>) max) -1 32768))
  (test-equal "get major number of two unsigned bytes (first case)"
    3 ((jit ctx (list <ubyte> <ubyte>) max) 2 3))
  (test-equal "get major number of two unsigned bytes (second case)"
    3 ((jit ctx (list <ubyte> <ubyte>) max) 3 2))
  (test-equal "get major number of two bytes (first case)"
    1 ((jit ctx (list <byte> <byte>) max) -1 1))
  (test-equal "get major number of two bytes (second case)"
    1 ((jit ctx (list <byte> <byte>) max) 1 -1))
  (let [(r (parameter <ubyte>))
        (a (parameter <ubyte>))
        (b (parameter <ubyte>))]
    (test-equal "handle lack of support for 8-bit conditional move"
      (list (SUB RSP 8) (MOV DL AL) (CMP DL SIL) (JNBE #x3) (MOV DL SIL) (ADD RSP 8) (RET))
      (resolve-jumps (jit-compile (attach (flatten-code ((term (max a b)) r)) (RET))))))
(test-end "binary max")
(test-end "aiscm operation")
