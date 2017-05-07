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
(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (oop goops)
             (rnrs bytevectors)
             (aiscm util)
             (aiscm asm)
             (aiscm variable)
             (aiscm command)
             (aiscm program)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm obj)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm bool)
             (aiscm rgb)
             (aiscm complex))


(test-begin "aiscm jit5")

(define ctx (make <context>))
(test-equal "multiply sequence with a number"
  '(6 9 15) (to-list (* (seq 2 3 5) 3)))
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
(test-equal "left-shift sequence"
  '(2 4 6) (to-list (<< (seq 1 2 3) 1)))
(test-equal "right-shift sequence"
  '(1 2 3) (to-list (>> (seq 4 8 12) 2)))
(test-equal "'duplicate' creates copy of slice"
  '(1 4) (to-list (duplicate (project (roll (arr (1 2 3) (4 5 6)))))))
(test-assert "'ensure-default-strides' should do nothing by default"
  (let [(m (make (multiarray <int> 2) #:shape '(6 4)))] (eq? m (ensure-default-strides m))))
(test-assert "'ensure-default-strides' should create a compact clone if the input is not contiguous"
  (let [(m (make (multiarray <int> 2) #:shape '(6 4)))] (equal? '(1 4) (strides (ensure-default-strides (roll m))))))
(test-equal "element-wise bit-wise and"
  '(3 0 1) (to-list (& (seq 3 4 5) 3)))
(test-equal "element-wise bit-wise or"
  '(3 7 7) (to-list (| 3 (seq 3 4 5))))
(test-equal "element-wise bit-wise xor"
  '(1 7 1) (to-list (^ (seq 2 3 4) (seq 3 4 5))))
(let [(a (var <int>))
      (r (var <bool>))]
  (test-equal "generate code for comparing with zero"
    (list (TEST a a) (SETE r)) (test-zero r a)))
(test-eq "typecast for scalar type"
  <int> (convert-type <int> <byte>))
(test-eq "typecast element-type of array type"
  (sequence <int>) (convert-type <int> (sequence <byte>)))
(test-equal "compare bytes with zero"
  '(#f #t #f) (to-list (=0 (seq -1 0 1))))
(test-equal "check whether bytes are not zero"
  '(#t #f #t) (to-list (!=0 (seq -1 0 1))))
(test-equal "element-wise and with three arguments"
  '(#f #t #f #f) (to-list (&& (seq #f #t #t #t) (seq #t #t #t #f) (seq #t #t #f #f))))
(test-equal "element-wise and with array and boolean argument"
  '(#f #t) (to-list (&& (seq #f #t) #t)))
(test-equal "element-wise or"
  '(#f #t #t #t) (to-list (|| (seq #f #t #f #t) (seq #f #f #t #t))))
(test-equal "element-wise not for booleans"
  '(#f #t #f) (to-list (! (seq #t #f #t))))
(test-equal "element-wise array-scalar comparison"
  '(#f #t #f) (to-list (= (seq <int> 1 2 3) 2)))
(test-equal "Element-wise scalar-array comparison"
  '(#f #t #f) (to-list (= 2 (seq <int> 1 2 3))))
(test-equal "Element-wise array-array comparison"
  '(#f #t #f) (to-list (= (seq <int> 3 2 1) (seq <int> 1 2 3))))
(test-equal "Element-wise comparison with integers of different size (first case)"
  '(#f #t #f) (to-list (= (seq <int> 3 2 1) (seq <byte> 1 2 3))))
(test-equal "Element-wise comparison with integers of different size (second case)"
  '(#f #t #f) (to-list (= (seq <byte> 3 2 1) (seq <int> 1 2 3))))
(test-equal "element-wise array-scalar non-equal comparison"
  '(#t #f #t) (to-list (!= (seq <int> 1 2 3) 2)))
(test-equal "element-wise lower-than"
  '(#t #f #f) (to-list (< (seq 3 4 255) 4)))
(test-equal "element-wise lower-than with signed values"
  '(#t #f #f) (to-list (< (seq -1 0 1) 0)))
(test-equal "element-wise lower-than with signed values"
  '(#f #f #t) (to-list (< 0 (seq -1 0 1))))
(test-equal "element-wise lower-equal with unsigned values"
  '(#t #t #f) (to-list (<= (seq 3 4 255) 4)))
(test-equal "element-wise lower-equal with signed values"
  '(#t #t #f) (to-list (<= (seq -1 0 1) 0)))
(test-equal "element-wise greater-than with signed values"
  '(#f #f #t) (to-list (> (seq 3 4 255) 4)))
(test-equal "element-wise lower-equal with signed values"
  '(#f #f #t) (to-list (> (seq -1 0 1) 0)))
(test-equal "element-wise greater-equal with unsigned values"
  '(#f #t #t) (to-list (>= (seq 3 4 255) 4)))
(test-equal "element-wise greater-equal with signed values"
  '(#f #t #t) (to-list (>= (seq -1 0 1) 0)))
(test-equal "element-wise lower-than with unsigned and signed byte"
  '(#f #f #f) (to-list (< (seq 1 2 128) -1)))
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
(test-equal "element-wise signed byte division"
  '(1 2 -3) (to-list (/ (seq 3 6 -9) 3)))
(test-equal "element-wise modulo"
  '(2 0 1) (to-list (% (seq 2 3 4) 3)))
(test-eq "check result type of parameter type conversion"
  <int> (type (to-type <int> (parameter <ubyte>))))
(test-equal "compile and run type conversion of scalar"
  42 ((jit ctx (list <byte>) (cut to-type <int> <>)) 42))
(test-equal "compile and run element-wise conversion of array to integer"
  '(2 3 5) (to-list ((jit ctx (list (sequence <byte>)) (cut to-type <int> <>)) (seq <byte> 2 3 5))))
(test-eq "type conversion uses specified element type for return value"
  <int> (typecode (to-type <int> (seq 2 3 5))))
(test-equal "type conversion preserves content"
  '(2 3 5) (to-list (to-type <int> (seq 2 3 5))))
(test-equal "compile and run element-wise conversion of array to byte"
  '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) (cut to-type <byte> <>)) (seq <int> 2 3 5))))
(test-equal "typecasting to smaller integer type"
  '(255 0 1) (to-list (to-type <ubyte> (seq 255 256 257))))
(let [(a   (skeleton <sint>))
      (tmp (skeleton <sint>))]
  (test-equal "Use intermediate value"
    (attach (code tmp a) tmp) (insert-intermediate a tmp list))
  (test-equal "Use empty code"
    (code tmp a) (insert-intermediate a tmp (const '()))))
(let* [(a (parameter <sint>))
       (f (~ a))]
  (test-assert "parameter is not a pointer"
    (not (is-pointer? (parameter <int>))))
  (test-assert "pointer parameter is a pointer"
    (is-pointer? (parameter (pointer <int>))))
  (test-assert "no conversion required if types are the same"
    (not (need-conversion? <sint> <sint>)))
  (test-assert "conversion required if types are different"
    (need-conversion? <int> <sint>))
  (test-assert "no conversion required if integers have the same size"
    (not (need-conversion? <byte> <ubyte>)))
  (test-assert "no conversion required when changing bytes to booleans"
    (not (need-conversion? <byte> <bool>)))
  (test-assert "no conversion required when changing boolean to bytes"
    (not (need-conversion? <bool> <ubyte>)))
  (test-assert "Compilation of function always requires intermediate value"
    (code-needs-intermediate? <sint> f))
  (test-assert "Value does not require intermediate value"
    (not (code-needs-intermediate? <sint> a)))
  (test-assert "Value of different size requires intermediate value"
    (code-needs-intermediate? <int> a))
  (test-assert "Compilation of function always requires intermediate value"
    (call-needs-intermediate? <sint> f))
  (test-assert "Value does not require intermediate value"
    (not (call-needs-intermediate? <sint> a)))
  (test-assert "Value of different size requires intermediate value"
    (call-needs-intermediate? <int> a))
  (test-assert "Pointer requires intermediate value"
    (call-needs-intermediate? <obj> (parameter (pointer <int>)))))
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
(let [(a (parameter <int>))
      (b (parameter <sint>))
      (c (parameter <ubyte>))
      (r (parameter <long>))]
  (test-skip 1)
  (test-equal "Coerce to output value when using multiple mutating operations"
    (list (SUB RSP 8) (MOVSX RSI EDX) (MOVSX RDX CX) (ADD RSI RDX) (MOVZX RCX AL) (ADD RSI RCX) (ADD RSP 8) (RET))
    (linear-scan-allocate (flatten-code (attach ((term (+ a b c)) r) (RET))))))
(test-equal "Compiling and run plus operation with three numbers"
  9 ((jit ctx (list <int> <int> <int>) +) 2 3 4))
(test-equal "Compile and run binary mutating operating with nested second parameter"
  9 ((jit ctx (list <int> <int> <int>) (lambda (x y z) (+ x (+ y z)))) 2 3 4))
(test-assert "compile and run unary functional operation with nested parameter"
  ((jit ctx (list <int> <int>) (lambda (x y) (=0 (+ x y)))) -3 3))
(test-assert "compile and run binary functional operation with nested first parameter"
  ((jit ctx (list <int> <int> <int>) (lambda (a b c) (= a (+ b c)))) 5 2 3))
(test-assert "compile and run binary functional operation with nested first parameter"
  ((jit ctx (list <int> <int> <int>) (lambda (a b c) (= (+ a b) c))) 2 3 5))
(let [(i (parameter <int>))]
  (test-equal "Integer decomposes to itself"
    i (decompose-value <int> i)))
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
(test-equal "get major number of two unsigned integers (first case)"
  32768 ((jit ctx (list <usint> <usint>) max) 32767 32768))
(test-equal "get major number of two unsigned integers (second case)"
  32768 ((jit ctx (list <usint> <usint>) max) 32768 32767))
(test-equal "get major number of two signed integers"
  1 ((jit ctx (list <sint> <sint>) max) -1 1))
(test-equal "get major number of signed and unsigned short integers"
  32768 ((jit ctx (list <sint> <usint>) max) -1 32768))
(test-equal "get minor number of two unsigned bytes (first case)"
  2 ((jit ctx (list <ubyte> <ubyte>) min) 2 3))
(test-equal "get minor number of two unsigned bytes (second case)"
  2 ((jit ctx (list <ubyte> <ubyte>) min) 3 2))
(test-equal "get major number of two unsigned bytes (first case)"
  3 ((jit ctx (list <ubyte> <ubyte>) max) 2 3))
(test-equal "get major number of two unsigned bytes (second case)"
  3 ((jit ctx (list <ubyte> <ubyte>) max) 3 2))
(test-equal "get minor number of two bytes (first case)"
  -1 ((jit ctx (list <byte> <byte>) min) -1 1))
(test-equal "get minor number of two bytes (second case)"
  -1 ((jit ctx (list <byte> <byte>) min) 1 -1))
(test-equal "get major number of two bytes (first case)"
  1 ((jit ctx (list <byte> <byte>) max) -1 1))
(test-equal "get major number of two bytes (second case)"
  1 ((jit ctx (list <byte> <byte>) max) 1 -1))
(let [(r (parameter <ubyte>))
      (a (parameter <ubyte>))
      (b (parameter <ubyte>))]
  (test-equal "handle lack of support for 8-bit conditional move"
    (list (SUB RSP 8) (MOV DL AL) (CMP DL SIL) (JNBE #x3) (MOV DL SIL) (ADD RSP 8) (RET))
    (resolve-jumps (linear-scan-allocate (attach (flatten-code ((term (max a b)) r)) (RET))))))
(test-equal "get minor number of signed bytes"
  -1 ((jit ctx (list <byte> <byte>) min) -1 1))

(test-end "aiscm jit5")
