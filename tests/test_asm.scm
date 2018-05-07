;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
             (system foreign)
             (aiscm asm)
             (aiscm mem)
             (aiscm element)
             (aiscm int)
             (aiscm pointer))

(test-begin "aiscm asm")

(load-extension "libguile-aiscm-tests" "init_tests")

(define guile-aiscm-tests (dynamic-link "libguile-aiscm-tests"))
(define jit-side-effect (dynamic-func "jit_side_effect" guile-aiscm-tests))

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
(define (bdata) (begin
                  (store bptr       b1)
                  (store (+ bptr 1) b2)
                  mem))
(define (wdata) (begin
                  (store wptr       w1)
                  (store (+ wptr 1) w2)
                  mem))
(define (idata) (begin
                  (store iptr       i1)
                  (store (+ iptr 1) i2)
                  mem))
(define (ldata) (begin
                  (store lptr       l1)
                  (store (+ lptr 1) l2)
                  mem))
(define (idx) (begin
                (store lptr #x0102030405060708)
                mem))

(test-eqv "Get code of RSP register"
  4 (get-code RSP))
(test-eqv "Get code of R8D register"
  8 (get-code R8D))
(test-eqv "Size of AL"
  1 (size-of AL))
(test-eqv "Size of AX"
  2 (size-of AX))
(test-eqv "Size of EAX"
  4 (size-of EAX))
(test-eqv "Size of RAX"
  8 (size-of RAX))
(test-equal "Display EAX"
  "EAX" (format #f "~a" EAX))
(test-equal "Retrieve RSP by register code and size"
  RSP (reg 8 4))
(test-equal "Retrieve R8D by register code and size"
  R8D (reg 4 8))
(test-equal "Retrieve EBX by type and register code"
  EBX (reg <int> 3))
(test-equal "Get corresponding register of different size"
  CX (to-type <sint> RCX))
(test-equal "Retrieve XMM5 using code"
  XMM5 (xmm 5))
(test-equal "Get code of XMM5"
  5 (get-code XMM5))
(test-equal "MOV EAX, 42"
  '(#xb8 #x2a #x00 #x00 #x00) (MOV EAX 42))
(test-equal "MOV ECX, 42"
  '(#xb9 #x2a #x00 #x00 #x00) (MOV ECX 42))
(test-equal "MOV R9D, 42"
  '(#x41 #xb9 #x2a #x00 #x00 #x00) (MOV R9D 42))
(test-assert "MOV RSI, 42"
  (equal? '(#x48 #xbe #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00) (MOV RSI 42)))
(test-assert "MOV R9, 42"
  (equal? '(#x49 #xb9 #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00) (MOV R9 42)))
(test-assert "Assign pointer address"
  (equal? '(#x48 #xbe #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00) (MOV RSI (make-pointer 42))))
(test-equal "MOV AL, 42"
  '(#xb0 #x2a) (MOV AL 42))
(test-equal "MOV AH, 42"
  '(#xb4 #x2a) (MOV AH 42))
(test-equal "MOV SPL, 42"
  '(#x40 #xb4 #x2a) (MOV SPL 42))
(test-equal "MOV AX, 42"
  '(#x66 #xb8 #x2a #x00) (MOV AX 42))
(test-equal "MOV EBX, EAX"
  '(#x8b #xd8) (MOV EBX EAX))
(test-equal "MOV ECX, EDX"
  '(#x8b #xca) (MOV ECX EDX))
(test-equal "MOV R8D, R9D"
  '(#x45 #x8b #xc1) (MOV R8D R9D))
(test-equal "MOV BL, AL"
  '(#x8a #xd8) (MOV BL AL))
(test-equal "MOV BX, AX"
  '(#x66 #x8b #xd8) (MOV BX AX))
(test-equal "MOV ECX, [RDX]"
  '(#x8b #x0a) (MOV ECX (ptr <int> RDX)))
(test-equal "Print pointer"
  "(ptr <int<32,signed>> RDX)" (format #f "~a" (ptr <int> RDX)))
(test-equal "type conversion for address"
  <sint> (slot-ref (to-type <sint> (ptr <int> RDX)) 'type))
(test-equal "MOV ECX, [RBP]; special case of REX encoding"
  '(#x8b #x4d #x00) (MOV ECX (ptr <int> RBP)))
(test-equal "MOV RCX, [RDX]"
  '(#x48 #x8b #x0a) (MOV RCX (ptr <long> RDX)))
(test-equal "MOV ECX, [R11]"
  '(#x41 #x8b #x0b) (MOV ECX (ptr <int> R11)))
(test-equal "MOV ECX, [R13]; special case of REX encoding"
  '(#x41 #x8b #x4d #x00) (MOV ECX (ptr <int> R13)))
(test-equal "MOV ECX, [RDX] + 4"
  '(#x8b #x4a #x04) (MOV ECX (ptr <int> RDX 4)))
(test-equal "Print pointer with displacement"
  "(ptr <int<32,signed>> RDX 4)" (format #f "~a" (ptr <int> RDX 4)))
(test-equal "MOV ECX, [RDX] + 128"
  '(#x8b #x8a #x80 #x00 #x00 #x00) (MOV ECX (ptr <int> RDX 128)))
(test-equal "MOV ECX, [RSP] + 4"
  '(#x8b #x4c #x24 #x04) (MOV ECX (ptr <int> RSP 4)))
(test-equal "MOV R10D, [R11] + 4"
  '(#x45 #x8b #x53 #x04) (MOV R10D (ptr <int> R11 4)))
(test-equal "MOV R10, [R11] + 4"
  '(#x4d #x8b #x53 #x04) (MOV R10 (ptr <long> R11 4)))
(test-equal "MOV CL, [RDX]"
  '(#x8a #x0a) (MOV CL (ptr <byte> RDX)))
(test-equal "MOV CX, [RDX]"
  '(#x66 #x8b #x0a) (MOV CX (ptr <sint> RDX)))
(test-equal "MOV [RCX], EDX"
  '(#x89 #x11) (MOV (ptr <int> RCX) EDX))
(test-equal "MOV [RCX], R8D"
  '(#x44 #x89 #x01) (MOV (ptr <int> RCX) R8D))
(test-equal "MOV [R10], EAX"
  '(#x41 #x89 #x02) (MOV (ptr <int> R10) EAX))
(test-equal "MOV [R12], EAX"
  '(#x41 #x89 #x04 #x24) (MOV (ptr <int> R12) EAX))
(test-equal "RET # near return"
  '(#xc3) (RET))
(test-equal "'obj' should return bytevector with machine code"
  #vu8(#x8b #xc7 #xc3) (obj (list (MOV EAX EDI) (RET))))
(test-assert "Empty function"
  (begin ((asm ctx <null> '() (list (RET)))) #t))
(test-eqv "Return constant in EAX"
  i1 ((asm ctx <int> '() (list (MOV EAX i1) (RET)))))
(test-eqv "Return constant in RAX"
  l1 ((asm ctx <long> '() (list (MOV RAX l1) (RET)))))
(test-eqv "Return constant in AL"
  b1 ((asm ctx <byte> '() (list (MOV AL b1) (RET)))))
(test-eqv "Return constant in AX"
  w1 ((asm ctx <sint> '() (list (MOV AX w1) (RET)))))
(test-eqv "Function copying content from ECX"
  i1 ((asm ctx <int> '() (list (MOV ECX i1) (MOV EAX ECX) (RET)))))
(test-eqv "Function copying content from R14D"
  i1 ((asm ctx <int> '() (list (MOV R14D i1) (MOV EAX R14D) (RET)))))
(test-eqv "Function copying content from R14"
  (ash 42 32) ((asm ctx <long> '() (list (MOV R14 (ash 42 32)) (MOV RAX R14) (RET)))))
(test-eqv "Function copying content from DIL"
  b1 ((asm ctx <int> '() (list (MOV DIL b1) (MOV AL DIL) (RET)))))
(test-equal "SHL EBP, 1"
  '(#xd1 #xe5) (SHL EBP))
(test-equal "SHL BPL, 1"
  '(#x40 #xd0 #xe5) (SHL BPL))
(test-equal "SHL BP, 1"
  '(#x66 #xd1 #xe5) (SHL BP))
(test-equal "SHL RBP, 1"
  '(#x48 #xd1 #xe5) (SHL RBP))
(test-equal "SHL BPL, CL"
  '(#x40 #xd2 #xe5) (SHL BPL CL))
(test-equal "SHL BP, CL"
  '(#x66 #xd3 #xe5) (SHL BP CL))
(test-equal "SAL EBP, 1"
  '(#xd1 #xe5) (SAL EBP))
(test-equal "SAL RBP, 1"
  '(#x48 #xd1 #xe5) (SAL RBP))
(test-equal "SAL EBP, CL"
  '(#xd3 #xe5) (SAL EBP CL))
(test-equal "SAL RBP, CL"
  '(#x48 #xd3 #xe5) (SAL RBP CL))
(test-eqv "Shift EAX left by 1"
  (ash i1 1) ((asm ctx <int> '() (list (MOV EAX i1) (SHL EAX) (RET)))))
(test-eqv "Shift R9D left by 1"
  (ash i1 1) ((asm ctx <int> '() (list (MOV R9D i1) (SHL R9D) (MOV EAX R9D) (RET)))))
(test-eqv "Function shifting 64-bit number left by 1"
  (ash l1 1) ((asm ctx <long> '() (list (MOV RAX l1) (SHL RAX) (RET)))))
(test-eqv "Shift left with generic amount"
  (ash i1 2) ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL 2) (SHL EAX CL) (RET)))))
(test-equal "SHR EBP, 1"
  '(#xd1 #xed) (SHR EBP))
(test-equal "SHR RBP, 1"
  '(#x48 #xd1 #xed) (SHR RBP))
(test-equal "SHR BL CL"
  '(#x40 #xd2 #xed) (SHR BPL CL))
(test-equal "SHR EBP, CL"
  '(#xd3 #xed) (SHR EBP CL))
(test-equal "SAR EBP, 1"
  '(#xd1 #xfd) (SAR EBP))
(test-equal "SAR RBP, 1"
  '(#x48 #xd1 #xfd) (SAR RBP))
(test-equal "SAR EBP, CL"
  '(#xd3 #xfd) (SAR EBP CL))
(test-equal "SAR RBP, CL"
  '(#x48 #xd3 #xfd) (SAR RBP CL))
(test-eqv "Function shifting right by 1"
  (ash i1 -1) ((asm ctx <int> '() (list (MOV EAX i1) (SHR EAX) (RET)))))
(test-eqv "Function shifting negative number right by 1"
  -21 ((asm ctx <int> '() (list (MOV EAX -42) (SAR EAX) (RET)))))
(test-eqv "Function shifting 64-bit number right by 1"
  (ash l1 -1) ((asm ctx <long> '() (list (MOV RAX l1) (SHR RAX) (RET)))))
(test-eqv "Shift left with generic amount"
  (ash i1 -2) ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL 2) (SHR EAX CL) (RET)))))
(test-eqv "Function shifting signed 64-bit number right by 2"
  (ash -1 30) ((asm ctx <long> '()
                    (list (MOV RAX (ash -1 32))
                          (SAR RAX)
                          (SAR RAX)
                          (RET)))))
(test-equal "ADD EAX, 13"
  '(#x05 #x0d #x00 #x00 #x00) (ADD EAX 13))
(test-equal "ADD RAX, 13"
  '(#x48 #x05 #x0d #x00 #x00 #x00) (ADD RAX 13))
(test-equal "ADD AX, 13"
  '(#x66 #x05 #x0d #x00) (ADD AX 13))
(test-equal "ADD AL, 13"
  '(#x04 #x0d) (ADD AL 13))
(test-equal "ADD ECX, 13"
  '(#x81 #xc1 #x0d #x00 #x00 #x00) (ADD ECX 13))
(test-equal "ADD RCX, 13"
  '(#x48 #x81 #xc1 #x0d #x00 #x00 #x00) (ADD RCX 13))
(test-equal "ADD CX, 13"
  '(#x66 #x81 #xc1 #x0d #x00) (ADD CX 13))
(test-equal "ADD CL, 13"
  '(#x80 #xc1 #x0d) (ADD CL 13))
(test-equal "ADD R10D, 13"
  '(#x41 #x81 #xc2 #x0d #x00 #x00 #x00) (ADD R10D 13))
(test-equal "ADD R10, 13"
  '(#x49 #x81 #xc2 #x0d #x00 #x00 #x00) (ADD R10 13))
(test-equal "ADD BYTE PTR [RDI], 42"
  '(#x80 #x07 #x2a) (ADD (ptr <byte> RDI) 42))
(test-equal "ADD ECX, EDX"
  '(#x03 #xca) (ADD ECX EDX))
(test-equal "ADD R14D, R15D"
  '(#x45 #x03 #xf7) (ADD R14D R15D))
(test-equal "ADD CX, DX"
  '(#x66 #x03 #xca) (ADD CX DX))
(test-equal "ADD CL, DL"
  '(#x02 #xca) (ADD CL DL))
(test-equal "ADD ECX, [RDX]"
  '(#x03 #x0a) (ADD ECX (ptr <int> RDX)))
(test-equal "SUB EAX, 13"
  '(#x2d #x0d #x00 #x00 #x00) (SUB EAX 13))
(test-equal "SUB RAX, 13"
  '(#x48 #x2d #x0d #x00 #x00 #x00) (SUB RAX 13))
(test-equal "SUB R10D, 13"
  '(#x41 #x81 #xea #x0d #x00 #x00 #x00) (SUB R10D 13))
(test-equal "SUB R10, 13"
  '(#x49 #x81 #xea #x0d #x00 #x00 #x00) (SUB R10 13))
(test-equal "SUB (ptr <int> R10), 13"
  '(#x41 #x81 #x2a #x0d #x00 #x00 #x00) (SUB (ptr <int> R10) 13))
(test-equal "SUB ECX, EDX"
  '(#x2b #xca) (SUB ECX EDX))
(test-equal "SUB R14D, R15D"
  '(#x45 #x2b #xf7) (SUB R14D R15D))
(test-eqv "Function using EAX to add 42 and 13"
  55 ((asm ctx <int> '() (list (MOV EAX 42) (ADD EAX 13) (RET)))))
(test-eqv "Function using RAX to add 42 and 13"
  55 ((asm ctx <long> '() (list (MOV RAX 42) (ADD RAX 13) (RET)))))
(test-eqv "Function using AX to add 42 and 13"
  55 ((asm ctx <sint> '()  (list (MOV AX 42) (ADD AX 13) (RET)))))
(test-eqv "Function using AL to add 42 and 13"
  55 ((asm ctx <byte> '() (list (MOV AL 42) (ADD AL 13) (RET)))))
(test-eqv "Function using R9D to add 42 and 13"
  55 ((asm ctx <int> '() (list (MOV R9D 42) (ADD R9D 13) (MOV EAX R9D) (RET)))))
(test-eqv "Function using R9 to add 42 and 13"
  55 ((asm ctx <long> '() (list (MOV R9 42) (ADD R9 13) (MOV RAX R9) (RET)))))
(test-eqv "Function using R9W to add 42 and 13"
  55 ((asm ctx <sint> '() (list (MOV R9W 42) (ADD R9W 13) (MOV AX R9W) (RET)))))
(test-eqv "Function using R9L to add 42 and 13"
  55 ((asm ctx <byte> '() (list (MOV R9L 42) (ADD R9L 13) (MOV AL R9L) (RET)))))
(test-eqv "Function adding two integers in EAX"
  (+ i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (ADD EAX i2) (RET)))))
(test-eqv "Function adding two integers in EDX"
  (+ i1 i2) ((asm ctx <int> '() (list (MOV EDX i1) (ADD EDX i2) (MOV EAX EDX) (RET)))))
(test-eqv "Function adding two integers in R10D"
  (+ i1 i2) ((asm ctx <int> '() (list (MOV R10D i1) (ADD R10D i2) (MOV EAX R10D) (RET)))))
(test-eqv "Function using EAX and ECX to add two integers"
  (+ i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (MOV ECX i2) (ADD EAX ECX) (RET)))))
(test-eqv "Function using R14D and R15D to add two integers"
  (+ i1 i2)
  ((asm ctx <int> '()
        (list (MOV R14D i1)
              (MOV R15D i2)
              (ADD R14D R15D)
              (MOV EAX R14D)
              (RET)))))
(test-eqv "Function using AX and CX to add two short integers"
  (+ w1 w2) ((asm ctx <sint> '() (list (MOV AX w1) (MOV CX w2) (ADD AX CX) (RET)))))
(test-eqv "Function using AL and CL to add two bytes"
  (+ b1 b2) ((asm ctx <byte> '() (list (MOV AL b1) (MOV CL b2) (ADD AL CL) (RET)))))
(test-eqv "Add integer memory operand"
  (+ i1 i2)
  ((asm ctx <int> '()
        (list (MOV EAX i2)
              (MOV RDX (idata))
              (ADD EAX (ptr <int> RDX))
              (RET)))))
(test-eqv "Add integer to integer in memory"
    (+ i1 i2)
    (begin ((asm ctx <null> '()
                 (list (MOV RSI mem)
                       (MOV (ptr <int> RSI) i1)
                       (ADD (ptr <int> RSI) i2)
                       (RET))))
           (get (fetch iptr))))
(test-equal "NOP # no operation"
  '(#x90) (NOP))
(test-eqv "Function with some NOP statements inside"
  i1 ((asm ctx <int> '() (list (MOV EAX i1) (NOP) (NOP) (RET)))))
(test-equal "PUSH RDX"
  '(#x48 #x52) (PUSH RDX))
(test-equal "PUSH RDI"
  '(#x48 #x57) (PUSH RDI))
(test-equal "PUSH R12"
  '(#x49 #x54) (PUSH R12))
(test-equal "POP RDX"
  '(#x48 #x5a) (POP RDX))
(test-equal "POP RDI"
  '(#x48 #x5f) (POP RDI))
(test-equal "POP R12"
  '(#x49 #x5c) (POP R12))
(test-eqv "Use 64-bit PUSH and POP"
  l1 ((asm ctx <long> '() (list (MOV RDX l1) (PUSH RDX) (POP RAX) (RET)))))
(test-eqv "Use 16-bit PUSH and POP"
  w1 ((asm ctx <sint> '() (list (MOV DX w1) (PUSH DX) (POP AX) (RET)))))
(test-eqv "Load integer from address in RCX"
  i1 ((asm ctx <int> '() (list (MOV RCX (idata)) (MOV EAX (ptr <int> RCX)) (RET)))))
(test-eqv "Load integer from address in R10"
  i1 ((asm ctx <int> '() (list (MOV R10 (idata)) (MOV EAX (ptr <int> R10)) (RET)))))
(test-eqv "Load integer from address in RCX with offset"
  i2 ((asm ctx <int> '() (list (MOV RCX (idata)) (MOV EAX (ptr <int> RCX 4)) (RET)))))
(test-eqv "Load integer from address in RCX with large offset"
    i1
    ((asm ctx <int> '()
          (list (MOV RCX (get (+ iptr 50)))
                (MOV EAX (ptr <int> RCX -200))
                (RET)))))
(test-eqv "Load integer from address in R9D with offset"
  i2 ((asm ctx <int> '() (list (MOV R9 (idata)) (MOV EAX (ptr <int> R9 4)) (RET)))))
(test-eqv "Load long integer from address in RCX"
  l1 ((asm ctx <long> '() (list (MOV RCX (ldata)) (MOV RAX (ptr <long> RCX)) (RET)))))
(test-eqv "Load long integer from address in RCX with offset"
  l2 ((asm ctx <long> '() (list (MOV RCX (ldata)) (MOV RAX (ptr <long> RCX 8)) (RET)))))
(test-eqv "Load short integer from address in RCX"
  w1 ((asm ctx <sint> '() (list (MOV RCX (wdata)) (MOV AX (ptr <sint> RCX)) (RET)))))
(test-eqv "Load short integer from address in RCX with offset"
  w2 ((asm ctx <sint> '() (list (MOV RCX (wdata)) (MOV AX (ptr <sint> RCX 2)) (RET)))))
(test-eqv "Load byte from address in RCX"
  b1 ((asm ctx <byte> '() (list (MOV RCX (bdata)) (MOV AL (ptr <byte> RCX)) (RET)))))
(test-eqv "Load byte from address in RCX with offset"
  b2 ((asm ctx <byte> '() (list (MOV RCX (bdata)) (MOV AL (ptr <byte> RCX 1)) (RET)))))
(test-equal "LEA RDX, [RCX + 4]"
  '(#x48 #x8d #x51 #x04) (LEA RDX (ptr <int> RCX 4)))
(test-equal "LEA RAX, [RSP - 4]"
  '(#x48 #x8d #x44 #x24 #xfc) (LEA RAX (ptr <int> RSP -4)))
(test-eqv "Load integer from address in RCX with offset using effective address"
    i2
    ((asm ctx <int> '()
          (list (MOV RCX (idata))
                (LEA RDX (ptr <int> RCX 4))
                (MOV EAX (ptr <int> RDX))
                (RET)))))
(test-equal "LEA RAX, [RDI + ESI * 4]"
  '(#x48 #x8d #x04 #xb7) (LEA RAX (ptr <int> RDI RSI)))
(test-equal "Print pointer with index"
  "(ptr <int<32,signed>> RDI RSI)" (format #f "~a" (ptr <int> RDI RSI)))
(test-eqv "Load integer from address in RCX with index times 4 using effective address"
  i2
  ((asm ctx <int> '()
        (list (MOV RCX (idata))
              (MOV RDI 1)
              (LEA RDX (ptr <int> RCX RDI))
              (MOV EAX (ptr <int> RDX))
              (RET)))))
(test-equal "LEA RAX, [RDI + ESI * 2]"
  '(#x48 #x8d #x04 #x77) (LEA RAX (ptr <sint> RDI RSI)))
(test-eqv "Load integer from address in RCX with index times 2 using effective address"
  i2
  ((asm ctx <int> '()
        (list (MOV RCX (idata))
              (MOV RDI 2)
              (LEA RDX (ptr <sint> RCX RDI))
              (MOV EAX (ptr <int> RDX))
              (RET)))))
(test-equal "LEA RAX, [RDI + ESI * 2 + 2]"
  '(#x48 #x8d #x44 #x77 #x02) (LEA RAX (ptr <sint> RDI RSI 2)))
(test-equal "Print pointer with index and offset"
  "(ptr <int<16,signed>> RDI RSI 2)" (format #f "~a" (ptr <sint> RDI RSI 2)))
(test-eqv "Load integer from address in RCX with index and offset using effective address"
  i2
  ((asm ctx <int> '()
        (list (MOV RCX (idata))
              (MOV RDI 3)
              (LEA RDX (ptr <byte> RCX RDI 1))
              (MOV EAX (ptr <int> RDX))
              (RET)))))
(test-eqv "Load integer from address in R9 with index and offset using effective address"
  i2
  ((asm ctx <int> '()
        (list (MOV R9 (idata))
              (MOV R10 3)
              (LEA R11 (ptr <byte> R9 R10 1))
              (MOV EAX (ptr <int> R11))
              (RET)))))
(test-eqv "Load 8-bit value from memory"
  #x08
  (begin ((asm ctx <long> '()
               (list (MOV RDI (idx))
                     (MOV AL (ptr <byte> RDI))
                     (RET))))))
(test-eqv "Load 16-bit value from memory"
  #x0708
  (begin ((asm ctx <long> '()
               (list (MOV RDI (idx))
                     (MOV AX (ptr <sint> RDI))
                     (RET))))))
(test-eqv "Load 32-bit value from memory"
  #x05060708
  (begin ((asm ctx <long> '()
               (list (MOV RDI (idx))
                     (MOV EAX (ptr <int> RDI))
                     (RET))))))
(test-eqv "Load 64-bit value from memory"
  #x0102030405060708
  (begin ((asm ctx <long> '()
               (list (MOV RDI (idx))
                     (MOV RAX (ptr <long> RDI))
                     (RET))))))
(test-eqv "Write value of ECX to memory"
  i1
  (begin ((asm ctx <null> '()
               (list (MOV RSI mem)
                     (MOV ECX i1)
                     (MOV (ptr <int> RSI) ECX)
                     (RET))))
          (get (fetch iptr))))
(test-eqv "Write value of RCX to memory"
  l1
  (begin ((asm ctx <null> '()
               (list (MOV RSI mem)
                     (MOV RCX l1)
                     (MOV (ptr <long> RSI) RCX)
                     (RET))))
          (get (fetch lptr))))
(test-eqv "Write value of R8D to memory"
  i1
  (begin ((asm ctx <int> '()
               (list (MOV RSI mem)
                     (MOV R8D i1)
                     (MOV (ptr <int> RSI) R8D)
                     (RET))))
          (get (fetch iptr))))
(test-eqv "Write value of ECX to memory"
  i1
  (begin ((asm ctx <null> '()
               (list (MOV RSI mem)
                     (MOV ECX i1)
                     (MOV (ptr <int> RSI) ECX)
                     (RET))))
          (get (fetch iptr))))
(test-equal "MOV DWORD PTR [RDI], 42"
  '(#xc7 #x07 #x2a #x00 #x00 #x00) (MOV (ptr <int> RDI) 42))
(test-equal "MOV QWORD PTR [RDI], 42"
  '(#x48 #xc7 #x07 #x2a #x00 #x00 #x00) (MOV (ptr <long> RDI) 42))
(test-equal "MOV WORD PTR [RDI], 42"
  '(#x66 #xc7 #x07 #x2a #x00) (MOV (ptr <sint> RDI) 42))
(test-equal "MOV BYTE PTR [RDI], 42"
  '(#xc6 #x07 #x2a) (MOV (ptr <byte> RDI) 42))
(test-eqv "Write 8-bit value to memory"
  #x0102030405060700
  (begin ((asm ctx <null> '()
               (list (MOV RDI (idx))
                     (MOV (ptr <byte> RDI) 0)
                     (RET))))
          (get (fetch lptr))))
(test-eqv "Write 16-bit value to memory"
  #x0102030405060000
  (begin ((asm ctx <null> '()
               (list (MOV R10 (idx))
                     (MOV (ptr <sint> R10) 0)
                     (RET))))
          (get (fetch lptr))))
(test-eqv "Write 32-bit value to memory"
  #x0102030400000000
  (begin ((asm ctx <null> '()
               (list (MOV RDI (idx))
                     (MOV (ptr <int> RDI) 0)
                     (RET))))
          (get (fetch lptr))))
(test-eqv "Write 64-bit value to memory"
  #x0000000000000000
  (begin ((asm ctx <null> '()
               (list (MOV RDI (idx))
                     (MOV (ptr <long> RDI) 0)
                     (RET))))
          (get (fetch lptr))))
(test-eqv "Return first integer argument"
  2 ((asm ctx <int> (make-list 4 <int>) (list (MOV EAX EDI) (RET))) 2 3 5 7))
(test-eqv "Return sixth integer argument"
  13 ((asm ctx <int> (make-list 6 <int>) (list (MOV EAX R9D) (RET))) 2 3 5 7 11 13))
(test-eqv "Return seventh integer argument"
  17 ((asm ctx <int> (make-list 8 <int>) (list (MOV EAX (ptr <int> RSP #x8)) (RET))) 2 3 5 7 11 13 17 19))
(test-eqv "Return eighth integer argument"
  19 ((asm ctx <int> (make-list 8 <int>) (list (MOV EAX (ptr <int> RSP #x10)) (RET))) 2 3 5 7 11 13 17 19))
(test-equal "NOT EBX"
  '(#xf7 #xd3) (NOT EBX))
(test-equal "NOT BX"
  '(#x66 #xf7 #xd3) (NOT BX))
(test-equal "NOT BL"
  '(#xf6 #xd3) (NOT BL))
(test-equal "NEG EBX"
  '(#xf7 #xdb) (NEG EBX))
(test-equal "NEG BX"
  '(#x66 #xf7 #xdb) (NEG BX))
(test-equal "NEG BL"
  '(#xf6 #xdb) (NEG BL))
(test-equal "INC EBX"
  '(#xff #xc3) (INC EBX))
(test-equal "INC BX"
  '(#x66 #xff #xc3) (INC BX))
(test-equal "INC BL"
  '(#xfe #xc3) (INC BL))
(test-eqv "Function negating an integer"
  (- i1) ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (NEG EAX) (RET))) i1))
(test-eqv "Function negating a long integer"
  (- l1) ((asm ctx <long> (list <long>) (list (MOV RAX RDI) (NEG RAX) (RET))) l1))
(test-eqv "Function negating a short integer"
  (- w1) ((asm ctx <sint> (list <sint>) (list (MOV AX DI) (NEG AX) (RET))) w1))
(test-eqv "Function negating a byte"
  (- b1) ((asm ctx <byte> (list <byte>) (list (MOV AL DIL) (NEG AL) (RET))) b1))
(test-eqv "Function subtracting two integers in EAX"
  (- i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (SUB EAX i2) (RET)))))
(test-eqv "Function subtracting two integers in EDX"
  (- i1 i2) ((asm ctx <int> '() (list (MOV EDX i1) (SUB EDX i2) (MOV EAX EDX) (RET)))))
(test-eqv "Function subtracting two integers in R10D"
  (- i1 i2) ((asm ctx <int> '() (list (MOV R10D i1) (SUB R10D i2) (MOV EAX R10D) (RET)))))
(test-eqv "Function using EAX and ECX to subtract two integers"
  (- i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (MOV ECX i2) (SUB EAX ECX) (RET)))))
(test-eqv "Function using R14D and R15D to subtract two integers"
  (- i1 i2)
  ((asm ctx <int> '()
        (list (MOV R14D i1)
              (MOV R15D i2)
              (SUB R14D R15D)
              (MOV EAX R14D)
              (RET)))))
(test-equal "IMUL RCX, RDX"
  '(#x48 #x0f #xaf #xca) (IMUL RCX RDX))
(test-eqv "Function using EAX and ECX to multiply two short integers"
  (* w1 w2) ((asm ctx <int> '() (list (MOV EAX w1) (MOV ECX w2) (IMUL EAX ECX) (RET)))))
(test-eqv "Function using RAX and RCX to multiply two integers"
  (* i1 i2) ((asm ctx <long> '() (list (MOV RAX i1) (MOV RCX i2) (IMUL RAX RCX) (RET)))))
(test-equal "IMUL EAX, R10D, 4"
  '(#x41 #x6b #xc2 #x04) (IMUL EAX R10D 4))
(test-equal "IMUL EAX, R10D, 1024"
  '(#x41 #x69 #xc2 #x00 #x04 #x00 #x00) (IMUL EAX R10D 1024))
(test-equal "IMUL EAX, R10D, -129"
  '(#x41 #x69 #xc2 #x7f #xff #xff #xff) (IMUL EAX R10D -129))
(test-equal "IMUL AX, R10W, 1024"
  '(#x66 #x41 #x69 #xc2 #x00 #x04) (IMUL AX R10W 1024))
(test-eqv "Function multiplying an integer with a byte constant"
  (* w1 b2) ((asm ctx <int> (list <int>) (list (IMUL EAX EDI b2) (RET))) w1))
(test-equal "IDIV ESI"
  '(#xf7 #xfe) (IDIV ESI))
(test-equal "IDIV SIL"
  '(#x40 #xf6 #xfe) (IDIV SIL))
(test-equal "IDIV SI"
  '(#x66 #xf7 #xfe) (IDIV SI))
(test-equal "DIV ESI"
  '(#xf7 #xf6) (DIV ESI))
(test-equal "DIV SIL"
  '(#x40 #xf6 #xf6) (DIV SIL))
(test-equal "DIV SI"
  '(#x66 #xf7 #xf6) (DIV SI))
(test-equal "AND EAX, 13"
  '(#x25 #x0d #x00 #x00 #x00) (AND EAX 13))
(test-equal "AND ECX, 13"
  '(#x81 #xe1 #x0d #x00 #x00 #x00) (AND ECX 13))
(test-equal "AND BYTE PTR [RDI], 42"
  '(#x80 #x27 #x2a) (AND (ptr <byte> RDI) 42))
(test-equal "AND ECX, EDX"
  '(#x23 #xca) (AND ECX EDX))
(test-equal "AND ECX, [RDX]"
  '(#x23 #x0a) (AND ECX (ptr <int> RDX)))
(test-equal "OR EAX, 13"
  '(#x0d #x0d #x00 #x00 #x00) (OR EAX 13))
(test-equal "OR ECX, 13"
  '(#x81 #xc9 #x0d #x00 #x00 #x00) (OR ECX 13))
(test-equal "OR BYTE PTR [RDI], 42"
  '(#x80 #x0f #x2a) (OR (ptr <byte> RDI) 42))
(test-equal "OR ECX, EDX"
  '(#x0b #xca) (OR ECX EDX))
(test-equal "OR ECX, [RDX]"
  '(#x0b #x0a) (OR ECX (ptr <int> RDX)))
(test-equal "XOR EAX, 13"
  '(#x35 #x0d #x00 #x00 #x00) (XOR EAX 13))
(test-equal "XOR ECX, 13"
  '(#x81 #xf1 #x0d #x00 #x00 #x00) (XOR ECX 13))
(test-equal "XOR BYTE PTR [RDI], 42"
  '(#x80 #x37 #x2a) (XOR (ptr <byte> RDI) 42))
(test-equal "XOR ECX, EDX"
  '(#x33 #xca) (XOR ECX EDX))
(test-equal "XOR ECX, [RDX]"
  '(#x33 #x0a) (XOR ECX (ptr <int> RDX)))
(test-assert "Assembler should tolerate labels"
  (asm ctx <int> '() (list (MOV EAX i1) 'tst (RET))))
(test-equal "JMP 42"
  '(#xeb #x2a) (JMP 42))
(test-equal "JMP 420"
  '(#xe9 #xa4 #x01 #x00 #x00) (JMP 420))
(test-equal "Remove label information from program"
  (list (NOP) (NOP)) (resolve-jumps (list (NOP) 'tst (NOP))))
(test-equal "Resolve jump address in trivial program"
  (list (JMP 0)) (resolve-jumps (list (JMP 'tst) 'tst)))
(test-equal "Resolve backward jump"
  (list (JMP -2)) (resolve-jumps (list 'tst (JMP 'tst))))
(test-assert "JMP is not a conditional jump"
  (not (conditional? (JMP 'tst))))
(test-equal "Resolve jump address in program with trailing NOP"
  (list (JMP 1) (NOP)) (resolve-jumps (list (JMP 'tst) (NOP) 'tst)))
(test-equal "Resolve jump address in program with leading NOP"
  (list (NOP) (JMP 0)) (resolve-jumps (list (NOP) (JMP 'tst) 'tst)))
(test-equal "Resolve multiple jump statements"
  (list (JMP 2) (JMP 0)) (resolve-jumps (list (JMP 'tst) (JMP 'tst) 'tst)))
(test-equal "Resolve multiple jump addresses"
  (list (JMP 2) (JMP -4)) (resolve-jumps (list 'a (JMP 'b) (JMP 'a) 'b)))
(test-equal "Resolve a jump of 127 bytes"
  (list #xeb #x7f) (car (resolve-jumps (append (list (JMP 'a)) (make-list 127 (NOP)) (list 'a)))))
(test-equal "Resolve a jump of 128 bytes"
  (list #xe9 #x80 #x00 #x00 #x00) (car (resolve-jumps (append (list (JMP 'a)) (make-list 128 (NOP)) (list 'a)))))
(test-equal "Resolve a jump over a long jump statement"
  (list #xeb #x05) (car (resolve-jumps (append (list (JMP 'a) (JMP 'b) 'a) (make-list 128 (NOP)) (list 'b)))))
(test-eqv "Function with a local jump"
  i1
  ((asm ctx <int> '()
        (list (MOV ECX i1)
              (JMP 'tst)
              (MOV ECX 0)
              'tst
              (MOV EAX ECX)
              (RET)))))
(test-eqv "Function with several local jumps"
  i1
  ((asm ctx <int> '()
        (list (MOV EAX 0)
              (JMP 'b)
              'a
              (MOV EAX i1)
              (JMP 'c)
              'b
              (MOV EAX i2)
              (JMP 'a)
              'c
              (RET)))))
(test-eq "'retarget' should update target of jump statement"
  'new (get-target (retarget (JMP 'old) 'new)))
(test-equal "CMP EAX 42"
  '(#x3d #x2a #x00 #x00 #x00) (CMP EAX 42))
(test-equal "CMP RAX 42"
  '(#x48 #x3d #x2a #x00 #x00 #x00) (CMP RAX 42))
(test-equal "CMP R10D 42"
  '(#x41 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10D 42))
(test-equal "CMP R10 42"
  '(#x49 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10 42))
(test-equal "SETE R9L"
  '(#x41 #x0f #x94 #xc1) (SETE R9L))
(test-equal "SETB DL"
  '(#x0f #x92 #xc2) (SETB DL))
(test-eqv "Compare zero in EAX with zero"
  1 ((asm ctx <byte> (list <int>) (list (MOV EAX EDI) (CMP EAX 0) (SETE AL) (RET))) 0))
(test-eqv "Compare non-zero number in EAX with zero"
  0 ((asm ctx <byte> (list <int>) (list (MOV EAX EDI) (CMP EAX 0) (SETE AL) (RET))) (logior 1 i1)))
(test-equal "CMP R10D 42"
  '(#x41 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10D 42))
(test-eqv "Compare zero in R10D with zero"
  1 ((asm ctx <byte> (list <int>) (list (MOV R10D EDI) (CMP R10D 0) (SETE AL) (RET))) 0))
(test-eqv "Compare non-zero number in R10D with zero"
  0 ((asm ctx <byte> (list <int>) (list (MOV R10D EDI) (CMP R10D 0) (SETE AL) (RET))) (logior 1 i1)))
(test-equal "CMP EDI ESI"
  '(#x3b #xfe) (CMP EDI ESI))
(test-eqv "Two integers being equal"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETE AL) (RET))) i1 i1))
(test-eqv "Two integers not being equal"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETE AL) (RET))) i1 (logxor 1 i1)))
(test-equal "CMP RSI RDI"
  '(#x48 #x3b #xf7) (CMP RSI RDI))
(test-eqv "Two long integers being equal"
  1 ((asm ctx <byte> (list <long> <long>) (list (CMP RSI RDI) (SETE AL) (RET))) l1 l1))
(test-eqv "Two long integers not being equal"
  0 ((asm ctx <byte> (list <long> <long>) (list (CMP RSI RDI) (SETE AL) (RET))) l1 (logxor 1 l1)))
(test-equal "SETB R9L"
  '(#x41 #x0f #x92 #xc1) (SETB R9L))
(test-eqv "Unsigned integer being below another"
  1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETB AL) (RET))) 1 3))
(test-eqv "Unsigned integer not being below another"
  0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETB AL) (RET))) 3 3))
(test-equal "SETNB R9L"
  '(#x41 #x0f #x93 #xc1) (SETNB R9L))
(test-eqv "Unsigned integer not being above or equal"
  0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNB AL) (RET))) 1 3))
(test-eqv "Unsigned integer being above or equal"
  1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNB AL) (RET))) 3 3))
(test-equal "SETNE R9L"
  '(#x41 #x0f #x95 #xc1) (SETNE R9L))
(test-eqv "Two integers not being unequal"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNE AL) (RET))) i1 i1))
(test-eqv "Two integers being unequal"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNE AL) (RET))) i1 (logxor 1 i1)))
(test-equal "SETBE R9L"
  '(#x41 #x0f #x96 #xc1) (SETBE R9L))
(test-eqv "Unsigned integer being below or equal"
  1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETBE AL) (RET))) 3 3))
(test-eqv "Unsigned integer not being below or equal"
  0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETBE AL) (RET))) 4 3))
(test-equal "SETNBE R9L"
  '(#x41 #x0f #x97 #xc1) (SETNBE R9L))
(test-eqv "Unsigned integer not being above"
  0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNBE AL) (RET))) 3 3))
(test-eqv "Unsigned integer being above"
  1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNBE AL) (RET))) 4 3))
(test-equal "SETL R9L"
  '(#x41 #x0f #x9c #xc1) (SETL R9L))
(test-eqv "Signed integer being less"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETL AL) (RET))) -2 3))
(test-eqv "Signed integer not being less"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETL AL) (RET))) 3 3))
(test-equal "SETNL R9L"
  '(#x41 #x0f #x9d #xc1) (SETNL R9L))
(test-eqv "Signed integer not being greater or equal"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNL AL) (RET))) -2 3))
(test-eqv "Signed integer being greater or equal"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNL AL) (RET))) 3 3))
(test-equal "SETLE R9L"
  '(#x41 #x0f #x9e #xc1) (SETLE R9L))
(test-eqv "Signed integer being less or equal"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETLE AL) (RET))) -2 -2))
(test-eqv "Signed integer not being less or equal"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETLE AL) (RET))) 3 -2))
(test-equal "SETNLE R9L"
  '(#x41 #x0f #x9f #xc1) (SETNLE R9L))
(test-eqv "Signed integer not being greater"
  0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNLE AL) (RET))) -2 -2))
(test-eqv "Signed integer being greater"
  1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNLE AL) (RET))) 3 -2))
(test-equal "JE 42"
  '(#x74 #x2a) (JE 42))
(test-assert "JE is a conditional jump"
  (conditional? (JE 'tst)))
(test-eqv "Test JE with ZF=1"
  1 ((asm ctx <int> '() (list (MOV EAX 1) (CMP EAX 1) (JE 'l) (MOV EAX 0) 'l (RET)))))
(test-eqv "Test JE with ZF=0"
  0 ((asm ctx <int> '() (list (MOV EAX 2) (CMP EAX 1) (JE 'l) (MOV EAX 0) 'l (RET)))))
(test-equal "JB 42"
  '(#x72 #x2a) (JB 42))
(test-eqv "Test JB with CF=1"
  3 ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (CMP EAX 5) (JB 'l) (MOV EAX 5) 'l (RET))) 3))
(test-eqv "Test JB with CF=0"
  5 ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (CMP EAX 5) (JB 'l) (MOV EAX 5) 'l (RET))) 7))
(test-equal "JNE 42"
  '(#x75 #x2a) (JNE 42))
(test-equal "JBE 42"
  '(#x76 #x2a) (JBE 42))
(test-equal "JNBE 42"
  '(#x77 #x2a) (JNBE 42))
(test-equal "JL 42"
  '(#x7c #x2a) (JL 42))
(test-equal "JNL 42"
  '(#x7d #x2a) (JNL 42))
(test-equal "JLE 42"
  '(#x7e #x2a) (JLE 42))
(test-equal "JNLE 42"
  '(#x7f #x2a) (JNLE 42))
(test-equal "TEST EAX 42"
  '(#xa9 #x2a #x00 #x00 #x00) (TEST EAX 42))
(test-equal "TEST ECX 42"
  '(#xf7 #xc1 #x2a #x00 #x00 #x00) (TEST ECX 42))
(test-equal "TEST EDI EDI"
  '(#x85 #xff) (TEST EDI EDI))
(test-equal "MOVSX CX DL"
  '(#x66 #x0f #xbe #xca) (MOVSX CX DL))
(test-eqv "Convert byte to short integer"
  b1 ((asm ctx <sint> '() (list (MOV AX w1) (MOV CL b1) (MOVSX AX CL) (RET)))))
(test-eqv "Convert negative byte to short integer"
  -42 ((asm ctx <sint> '() (list (MOV AX w1) (MOV CL -42) (MOVSX AX CL) (RET)))))
(test-equal "MOVSX ECX DL"
  '(#x0f #xbe #xca) (MOVSX ECX DL))
(test-eqv "Convert byte to integer"
  b1 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CL b1) (MOVSX EAX CL) (RET)))))
(test-eqv "Convert negative byte to integer"
  -42 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CL -42) (MOVSX EAX CL) (RET)))))
(test-equal "MOVSX RCX DL"
  '(#x48 #x0f #xbe #xca) (MOVSX RCX DL))
(test-eqv "Convert byte to short integer"
  b1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CL b1) (MOVSX RAX CL) (RET)))))
(test-eqv "Convert negative byte to short integer"
  -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CL -42) (MOVSX RAX CL) (RET)))))
(test-equal "MOVSX ECX DX"
  '(#x0f #xbf #xca) (MOVSX ECX DX))
(test-eqv "Convert short integer to integer"
  w1 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CX w1) (MOVSX EAX CX) (RET)))))
(test-eqv "Convert negative short integer to integer"
  -42 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CX -42) (MOVSX EAX CX) (RET)))))
(test-equal "MOVSX RCX DX"
  '(#x48 #x0f #xbf #xca) (MOVSX RCX DX))
(test-eqv "Convert short integer to long integer"
  w1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CX w1) (MOVSX RAX CX) (RET)))))
(test-eqv "Convert negative short integer to long integer"
  -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CX -42) (MOVSX RAX CX) (RET)))))
(test-equal "MOVSX RCX EDX"
  '(#x48 #x63 #xca) (MOVSX RCX EDX))
(test-eqv "Convert integer to long integer"
  i1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV ECX i1) (MOVSX RAX ECX) (RET)))))
(test-eqv "Convert negative integer to long integer"
  -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV ECX -42) (MOVSX RAX ECX) (RET)))))
(test-equal "MOVZX CX AL"
  '(#x66 #x0f #xb6 #xc8) (MOVZX CX AL))
(test-eqv "Convert unsigned byte to unsigned short integer"
  b1 ((asm ctx <usint> '() (list (MOV AX w1) (MOV CL b1) (MOVZX AX CL) (RET)))))
(test-equal "MOVZX ECX AL"
  '(#x0f #xb6 #xc8) (MOVZX ECX AL))
(test-eqv "Convert unsigned byte to unsigned integer"
  b1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL b1) (MOVZX EAX CL) (RET)))))
(test-equal "MOVZX ECX AX"
  '(#x0f #xb7 #xc8) (MOVZX ECX AX))
(test-eqv "Convert unsigned short integer to unsigned integer"
  w1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CX w1) (MOVZX EAX CX) (RET)))))
(test-equal "MOVZX ECX AX"
  '(#x0f #xb7 #xc8) (MOVZX ECX AX))
(test-eqv "Convert unsigned short integer to unsigned integer"
  w1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CX w1) (MOVZX EAX CX) (RET)))))
(test-equal "MOVZX ECX AX"
  '(#x0f #xb7 #xc8) (MOVZX ECX AX))
(test-error "MOVZX RCX EAX should throw error"
  'misc-error (MOVZX RCX EAX))
(test-eqv "Convert unsigned short integer to unsigned long integer"
  w1 ((asm ctx <ulong> '() (list (MOV RAX l1) (MOV CX w1) (MOVZX RAX CX) (RET)))))
(test-eqv "Convert unsigned integer to unsigned long integer"
  i1 ((asm ctx <ulong> '() (list (MOV RAX l1) (MOV ECX i1) (MOV RAX ECX) (RET)))))
(test-eqv "Save and restore value of RBX using the stack (this will crash if it does not restore RBX properly)"
  42 ((asm ctx <int> '() (list (PUSH RBX) (MOV EBX 42) (MOV EAX EBX) (POP RBX) (RET)))))
(test-equal "CBW"
  '(#x66 #x98) (CBW))
(test-equal "CWDE"
  '(#x98) (CWDE))
(test-equal "CDQE"
  '(#x48 #x98) (CDQE))
(test-eqv "Sign-extend AL"
  (- b1) ((asm ctx <sint> '() (list (MOV AX w1) (MOV AL (- b1)) (CBW) (RET)))))
(test-eqv "Sign-extend AX"
  (- w1) ((asm ctx <int> '() (list (MOV EAX i1) (MOV AX (- w1)) (CWDE) (RET)))))
(test-eqv "Sign-extend EAX"
  (- i1) ((asm ctx <long> '() (list (MOV RAX l1) (MOV EAX (- i1)) (CDQE) (RET)))))
(test-equal "CWD"
  '(#x66 #x99) (CWD))
(test-equal "CDQ"
  '(#x99) (CDQ))
(test-equal "CQO"
  '(#x48 #x99) (CQO))
(test-eqv "Sign extend AX into DX:AX"
  #xffff ((asm ctx <usint> '() (list (MOV AX -42) (CWD) (MOV AX DX) (RET)))))
(test-eqv "Sign extend EAX into EDX:EAX"
  #xffffffff ((asm ctx <uint> '() (list (MOV EAX -42) (CDQ) (MOV EAX EDX) (RET)))))
(test-eqv "Sign extend RAX into RDX:RAX"
  #xffffffffffffffff ((asm ctx <ulong> '() (list (MOV RAX -42) (CQO) (MOV RAX RDX) (RET)))))
(test-eqv "Explicitely manage stack pointer (this will crash if it does not restore RBX and RSP properly)"
  42
  ((asm ctx <int> '()
        (list (MOV (ptr <long> RSP -8) RBX)
              (SUB RSP 8)
              (MOV EBX 42)
              (MOV EAX EBX)
              (MOV RBX (ptr <long> RSP))
              (ADD RSP 8)
              (RET)))))
(test-eqv "Check whether this Guile version supports foreign calls with more than 10 arguments"
  11
  ((asm ctx
        <int>
        (make-list 11 <int>)
        (list (MOV EAX (ptr <int> RSP #x28)) (RET)))
   1 2 3 4 5 6 7 8 9 10 11))
(test-equal "CMOVB ECX ESI"
  '(#x0f #x42 #xce) (CMOVB ECX ESI))
(test-equal "CMOVNB ECX ESI"
  '(#x0f #x43 #xce) (CMOVNB ECX ESI))
(test-equal "CALL RDX"
  '(#x48 #xff #xd2) (CALL RDX))
(test-eqv "Compile a method call"
  42
  (begin (jit-reset-side-effect)
         ((asm ctx <null> '() (list (MOV RCX jit-side-effect) (CALL RCX) (RET))))
         (jit-reset-side-effect)))

(test-end "aiscm asm")
