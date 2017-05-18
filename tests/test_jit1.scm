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
             (aiscm operation)
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
(define b1 (random (ash 1  6)))
(define b2 (random (ash 1  6)))
(define w1 (random (ash 1 14)))
(define w2 (random (ash 1 14)))
(define i1 (random (ash 1 30)))
(define i2 (random (ash 1 30)))
(define l1 (random (ash 1 62)))
(define l2 (random (ash 1 62)))
(define mem (make <mem> #:size 256))
(define bptr (make (pointer <byte>) #:value mem))
(define wptr (make (pointer <sint>) #:value mem))
(define iptr (make (pointer <int>) #:value mem))
(define lptr (make (pointer <long>) #:value mem))
(define (idata) (begin
                  (store iptr       i1)
                  (store (+ iptr 1) i2)
                  iptr))

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

(test-eqv "compile and run integer identity function"
  42 ((jit ctx (list <int>) identity) 42))
(test-eqv "compile and run boolean identity function"
  #t ((jit ctx (list <bool>) identity) #t))
(test-equal "compile and run identity function for array"
  '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) identity) (seq <int> 2 3 5))))
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

(test-begin "filling arrays")
  (test-equal "fill byte sequence"
    '(3 3 3) (to-list (fill <byte> '(3) 3)))
  (test-equal "fill integer sequence"
    '(4 4 4) (to-list (fill <int> '(3) 4)))
  (test-equal "fill 2D array"
    '((5 5 5) (5 5 5)) (to-list (fill <int> '(3 2) 5)))
  (test-equal "fill RGB sequence"
    (list (rgb 2 3 5) (rgb 2 3 5)) (to-list (fill <intrgb> '(2) (rgb 2 3 5))))
(test-end "filling arrays")

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
(test-equal "Negate integer"
  -42 ((jit ctx (list <int>) -) 42))
(test-equal "compile and run function for negating array"
  '(-2 3 -5) (to-list ((jit ctx (list (sequence <int>)) -) (seq <int> 2 -3 5))))
(test-equal "plus passes through values"
  42 ((jit ctx (list <int>) +) 42))
(test-equal "Compiling a plus operation with different types creates an equivalent machine program"
  3 ((jit ctx (list <int> <sint> <ubyte>) +) 2 -3 4))
(test-equal "Compile and run code for fetching data from a pointer"
  i1 ((jit ctx (list (pointer <int>)) identity) (idata)))
(test-equal "Bitwise not of sequence"
  '(253 252 250) (to-list ((jit ctx (list (sequence <ubyte>)) ~) (seq 2 3 5))))
(test-equal "Subtract byte from integer sequence"
  '(0 1 2) (to-list ((jit ctx (list (sequence <int>) <byte>) -) (seq <int> 1 2 3) 1)))
(test-equal "Multiply integer sequence with an integer"
  '(2 4 6) (to-list ((jit ctx (list (sequence <int>) <int>) *) (seq <int> 1 2 3) 2)))
(test-equal "negate integer sequence"
  '(-2 -3 -5) (to-list (- (seq <int> 2 3 5))))
(test-equal "negate 2D array"
  '((-1 2) (3 -4)) (to-list (- (arr (1 -2) (-3 4)))))
(test-equal "Negate integer twice"
  42 ((jit ctx (list <int>) (compose - -)) 42))
(test-equal "add 1 to downsampled array"
  '(2 4) (to-list (+ (downsample 2 (seq 1 2 3 4)) 1)))
(test-equal "add downsampled array to 1"
  '(2 4) (to-list (+ 1 (downsample 2 (seq 1 2 3 4)))))
(test-equal "add two downsampled arrays"
  '(2 6) (let [(s (downsample 2 (seq 1 2 3 4)))] (to-list (+ s s))))
(test-equal "unary plus for sequence"
  '(2 3 5) (to-list (+ (seq <int> 2 3 5))))
(test-equal "bitwise negation of array"
  '(253 252 250) (to-list (~ (seq 2 3 5))))
(test-equal "add integer to integer sequence"
  '(3 4 6) (to-list (+ (seq 2 3 5) 1)))
(test-equal "add integer sequence to integer"
  '(3 4 6) (to-list (+ 1 (seq 2 3 5))))
(test-equal "add two sequences"
  '(3 5 9) (to-list (+ (seq 2 3 5) (seq 1 2 4))))
(test-equal "element-wise subtract integer from a sequence"
  '(-3 -2 -1) (to-list (- (seq -1 0 1) 2)))
(test-equal "subtract 1 from a 2D array"
  '((0 1 2) (3 4 5)) (to-list (- (arr (1 2 3) (4 5 6)) 1)))
(test-equal "subtract 2D array from integer"
  '((6 5 4) (3 2 1)) (to-list (- 7 (arr (1 2 3) (4 5 6)))))
(test-end "aiscm jit1")
