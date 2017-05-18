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

(test-begin "negate number")
  (let [(out (skeleton <int>))
        (a   (skeleton <int>))]
    (test-equal "generate code for negating number"
      (list (list (mov-signed (get out) (get a))) (NEG (get out))) (code (parameter out) (- (parameter a)))))
(test-end "negate number")
(test-end "aiscm operation")
