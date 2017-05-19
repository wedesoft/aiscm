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

(test-equal "Compile and run code for fetching data from a pointer"
  i1 ((jit ctx (list (pointer <int>)) identity) (idata)))

(test-equal "negate integer sequence"
  '(-2 -3 -5) (to-list (- (seq <int> 2 3 5))))
(test-equal "negate 2D array"
  '((-1 2) (3 -4)) (to-list (- (arr (1 -2) (-3 4)))))
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

(test-equal "subtract 2D array from each other"
  '((1 1 2) (3 4 5)) (to-list (- (arr (2 3 5) (7 9 11)) (arr (1 2 3) (4 5 6)))))
(test-equal "negate 3D array"
  '(((-1 2 -3) (4 -5 6))) (to-list (- (arr ((1 -2 3) (-4 5 -6))))))
(test-equal "add 1D and 2D array"
  '((3 4 5) (7 8 9)) (to-list (+ (seq 0 1) (arr (3 4 5) (6 7 8)))))
(test-equal "add 2D and 1D array"
  '((3 4 5) (7 8 9)) (to-list (+ (arr (3 4 5) (6 7 8)) (seq 0 1))))
(test-equal "add scalar to 3D array"
  '(((2 3 4) (5 6 7))) (to-list (+ (arr ((1 2 3) (4 5 6))) 1)))
(test-equal "add 3D array to scalar"
  '(((2 3 4) (5 6 7))) (to-list (+ 1 (arr ((1 2 3) (4 5 6))))))
(test-equal "add two 3D arrays"
  '(((2 4 6) (8 10 12))) (let [(m (arr ((1 2 3) (4 5 6))))] (to-list (+ m m))))
(test-equal "add 1 to 4D array"
  '((((3 3) (3 3)) ((3 3) (3 3))) (((3 3) (3 3)) ((3 3) (3 3))))
  (to-list (+ (arr (((2 2) (2 2)) ((2 2) (2 2))) (((2 2) (2 2)) ((2 2) (2 2)))) 1)))
(test-equal "add unsigned integer and integer array"
  '(1 2 3) (to-list (+ (seq <uint> 1 2 3) (seq <int> 0 0 0))))
(test-end "aiscm jit1")
