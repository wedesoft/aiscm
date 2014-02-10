(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:use-module (aiscm mem)
  #:export (<jit-context>
            <reg<>>
            <reg<32>>
            <reg<64>>
            asm
            ADD JMP MOV NOP RET PUSH POP SAL SAR SHL SHR
            EAX ECX EDX EBX ESP EBP ESI EDI
            R8W R9W R10W R11W R12W R13W R14W R15W
            RAX RCX RDX RBX RSP RBP RSI RDI
            R8D R9D R10D R11D R12D R13D R14D R15D
            *RAX *RCX *RDX *RBX *RSP *disp32 *RSI *RDI))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> ()
  (binaries #:init-value '()))
(define-method (asm (self <jit-context>) return_type commands . args)
  (let ((code (make-mmap (u8-list->bytevector (apply append commands)))))
    (slot-set! self 'binaries
               (cons code (slot-ref self 'binaries)))
    (pointer->procedure return_type
                        (make-pointer (mmap-address code))
                        args)))
(define-class <reg<>> () (code #:init-keyword #:code #:getter get-code))
(define-class <reg<32>> (<reg<>>))
(define  EAX (make <reg<32>> #:code #b0000))
(define  ECX (make <reg<32>> #:code #b0001))
(define  EDX (make <reg<32>> #:code #b0010))
(define  EBX (make <reg<32>> #:code #b0011))
(define  ESP (make <reg<32>> #:code #b0100))
(define  EBP (make <reg<32>> #:code #b0101))
(define  ESI (make <reg<32>> #:code #b0110))
(define  EDI (make <reg<32>> #:code #b0111))
(define  R8W (make <reg<32>> #:code #b1000))
(define  R9W (make <reg<32>> #:code #b1001))
(define R10W (make <reg<32>> #:code #b1010))
(define R11W (make <reg<32>> #:code #b1011))
(define R12W (make <reg<32>> #:code #b1100))
(define R13W (make <reg<32>> #:code #b1101))
(define R14W (make <reg<32>> #:code #b1110))
(define R15W (make <reg<32>> #:code #b1111))
(define-class <reg<64>> (<reg<>>))
(define  RAX (make <reg<64>> #:code #b0000))
(define  RCX (make <reg<64>> #:code #b0001))
(define  RDX (make <reg<64>> #:code #b0010))
(define  RBX (make <reg<64>> #:code #b0011))
(define  RSP (make <reg<64>> #:code #b0100))
(define  RBP (make <reg<64>> #:code #b0101))
(define  RSI (make <reg<64>> #:code #b0110))
(define  RDI (make <reg<64>> #:code #b0111))
(define  R8D (make <reg<64>> #:code #b1000))
(define  R9D (make <reg<64>> #:code #b1001))
(define R10D (make <reg<64>> #:code #b1010))
(define R11D (make <reg<64>> #:code #b1011))
(define R12D (make <reg<64>> #:code #b1100))
(define R13D (make <reg<64>> #:code #b1101))
(define R14D (make <reg<64>> #:code #b1110))
(define R15D (make <reg<64>> #:code #b1111))
(define-class <address> (<reg<64>>))
(define *RAX    (make <address> #:code #b000))
(define *RCX    (make <address> #:code #b001))
(define *RDX    (make <address> #:code #b010))
(define *RBX    (make <address> #:code #b011))
(define *RSP    (make <address> #:code #b100))
(define *disp32 (make <address> #:code #b101))
(define *RSI    (make <address> #:code #b110))
(define *RDI    (make <address> #:code #b111))
(define (raw imm bits)
  (bytevector->u8-list (pack (make (integer bits unsigned) #:value imm))))
(define (ptr->int ptr)
  (pointer-address (get-memory ptr)))
(define-method (bits3 (reg <integer>)) (logand reg #b111))
(define-method (bits3 (reg <reg<>>)) (bits3 (get-code reg)))
(define-method (bit4 (reg <integer>)) (logand reg #b1))
(define-method (bit4 (reg <reg<>>)) (bit4 (ash (get-code reg) -3)))
(define* (opcode code #:optional reg)
  (list (logior code (if reg (bits3 reg) 0))))
(define (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash (bits3 reg/opcode) 3) (bits3 r/m))))
(define (REX W R X B)
  (let ((flags (logior
                 (ash W 3)
                 (ash (bit4 R) 2)
                 (ash (bit4 X) 1)
                 (bit4 B))))
    (if (zero? flags) '() (list (logior (ash #b0100 4) flags)))))
(define (SIB SS index r)
  (list (logior (ash SS 6) (ash index 3) (bits3 r))))
(define-method (MOV (r/m32 <reg<32>>) (r32 <reg<32>>))
  (append (REX 0 r32 0 r/m32) (opcode #x89) (ModR/M #b11 r32 r/m32)))
(define-method (MOV (r/m64 <reg<64>>) (r64 <reg<64>>))
  (append (REX 1 r64 0 r/m64) (opcode #x89) (ModR/M #b11 r64 r/m64)))
(define-method (MOV (r/m32 <address>) (r32 <reg<32>>))
  (append (REX 0 r32 0 r/m32) (opcode #x89) (ModR/M #b00 r32 r/m32)))
(define-method (MOV (r32 <reg<32>>) (imm32 <integer>))
  (append (REX 0 0 0 r32) (opcode #xb8 r32) (raw imm32 32)))
(define-method (MOV (r64 <reg<64>>) (imm64 <integer>))
  (append (REX 1 0 0 r64) (opcode #xb8 r64) (raw imm64 64)))
(define-method (MOV (r32 <reg<32>>) (imm32 <mem>))
  (MOV r32 (ptr->int imm32)))
(define-method (MOV (r64 <reg<64>>) (imm64 <mem>))
  (MOV r64 (ptr->int imm64)))
(define-method (MOV (r32 <reg<32>>) (r/m32 <address>))
  (append (opcode #x8b) (ModR/M #b00 r32 r/m32))); TODO: REX
(define-method (MOV (r32 <reg<32>>) (r/m32 <address>) (disp <integer>))
  (let ((sib (if (equal? r/m32 *RSP) (SIB #b00 #b100 r/m32) '())))
    (append (opcode #x8b) (ModR/M #b01 r32 r/m32) sib (raw disp 8)))); TODO: REX
(define (NOP) '(#x90))
(define (RET) '(#xc3))
(define-method (SHL (r/m32 <reg<32>>))
  (append (opcode #xd1) (ModR/M #b11 4 r/m32))); TODO: REX
(define-method (SHL (r/m64 <reg<64>>))
  (append (REX 1 0 0 0) (opcode #xd1) (ModR/M #b11 4 r/m64))); TODO: fix REX
(define-method (SHR (r/m32 <reg<32>>))
  (append (opcode #xd1) (ModR/M #b11 5 r/m32))); TODO: REX
(define-method (SHR (r/m64 <reg<64>>))
  (append (REX 1 0 0 0) (opcode #xd1) (ModR/M #b11 5 r/m64))); TODO: fix REX
(define-method (SAL (r/m32 <reg<32>>))
  (append (opcode #xd1) (ModR/M #b11 4 r/m32))); TODO: REX
(define-method (SAL (r/m64 <reg<64>>))
  (append (REX 1 0 0 0) (opcode #xd1) (ModR/M #b11 4 r/m64))); TODO: fix REX
(define-method (SAR (r/m32 <reg<32>>))
  (append (opcode #xd1) (ModR/M #b11 7 r/m32))); TODO: REX
(define-method (SAR (r/m64 <reg<64>>))
  (append (REX 1 0 0 0) (opcode #xd1) (ModR/M #b11 7 r/m64))); TODO: fix REX
(define-method (ADD (r/m32 <reg<32>>) (r32 <reg<32>>))
  (append (opcode #x01) (ModR/M #b11 r32 r/m32))); TODO: REX
(define-method (ADD (r/m32 <reg<32>>) (imm32 <integer>))
  (if (equal? r/m32 EAX)
    (append (opcode #x05) (raw imm32 32))
    (append (opcode #x81) (ModR/M #b11 0 r/m32) (raw imm32 32)))); TODO: REX
(define-method (ADD (r/m64 <reg<64>>) (imm32 <integer>))
  (if (equal? r/m64 RAX)
    (append (REX 1 0 0 0) (opcode #x05) (raw imm32 32))
    (append (REX 1 0 0 0) (opcode #x81) (ModR/M #b11 0 r/m64) (raw imm32 32)))); TODO: fix REX
(define-method (PUSH (r32 <reg<32>>))
  (opcode #x50 r32)); TODO: REX
(define-method (POP (r32 <reg<32>>))
  (opcode #x58 r32)); TODO: REX
(define-method (JMP (rel32 <integer>))
  (append (opcode #xe9) (raw rel32 32)))
