(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:export (<jit-context>
            <reg<>>
            <reg<32>>
            <reg<64>>
            asm
            ADD JMP MOV NOP RET PUSH POP SAL SAR SHL SHR NEG SUB
            EAX ECX EDX EBX ESP EBP ESI EDI
            R8D R9D R10D R11D R12D R13D R14D R15D
            RAX RCX RDX RBX RSP RBP RSI RDI
            R8 R9 R10 R11 R12 R13 R14 R15
            *RAX *RCX *RDX *RBX *RSP *disp32 *RSI *RDI
            *R8 *R9 *R10 *R11 *R12 *R13 *R14 *R15)
  #:export-syntax (label))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> ()
  (binaries #:init-value '()))
(define-method (asm (self <jit-context>) return_type commands . args)
  (let* ((label? (lambda (x) (eq? (car x) 'label)))
         (nolabels (filter (negate label?) commands))
         (code (make-mmap (u8-list->bytevector (apply append nolabels)))))
    (slot-set! self 'binaries (cons code (slot-ref self 'binaries)))
    (pointer->procedure return_type (make-pointer (mmap-address code)) args)))
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
(define  R8D (make <reg<32>> #:code #b1000))
(define  R9D (make <reg<32>> #:code #b1001))
(define R10D (make <reg<32>> #:code #b1010))
(define R11D (make <reg<32>> #:code #b1011))
(define R12D (make <reg<32>> #:code #b1100))
(define R13D (make <reg<32>> #:code #b1101))
(define R14D (make <reg<32>> #:code #b1110))
(define R15D (make <reg<32>> #:code #b1111))
(define-class <reg<64>> (<reg<>>))
(define RAX (make <reg<64>> #:code #b0000))
(define RCX (make <reg<64>> #:code #b0001))
(define RDX (make <reg<64>> #:code #b0010))
(define RBX (make <reg<64>> #:code #b0011))
(define RSP (make <reg<64>> #:code #b0100))
(define RBP (make <reg<64>> #:code #b0101))
(define RSI (make <reg<64>> #:code #b0110))
(define RDI (make <reg<64>> #:code #b0111))
(define R8  (make <reg<64>> #:code #b1000))
(define R9  (make <reg<64>> #:code #b1001))
(define R10 (make <reg<64>> #:code #b1010))
(define R11 (make <reg<64>> #:code #b1011))
(define R12 (make <reg<64>> #:code #b1100))
(define R13 (make <reg<64>> #:code #b1101))
(define R14 (make <reg<64>> #:code #b1110))
(define R15 (make <reg<64>> #:code #b1111))
(define-class <address> (<reg<64>>))
(define *RAX    (make <address> #:code #b0000))
(define *RCX    (make <address> #:code #b0001))
(define *RDX    (make <address> #:code #b0010))
(define *RBX    (make <address> #:code #b0011))
(define *RSP    (make <address> #:code #b0100))
(define *disp32 (make <address> #:code #b0101))
(define *RSI    (make <address> #:code #b0110))
(define *RDI    (make <address> #:code #b0111))
(define *R8     (make <address> #:code #b1000))
(define *R9     (make <address> #:code #b1001))
(define *R10    (make <address> #:code #b1010))
(define *R11    (make <address> #:code #b1011))
(define *R12    (make <address> #:code #b1100))
(define *R13    (make <address> #:code #b1101))
(define *R14    (make <address> #:code #b1110))
(define *R15    (make <address> #:code #b1111))
(define-method (raw (imm <integer>) (bits <integer>))
  (bytevector->u8-list (pack (make (integer bits unsigned) #:value imm))))
(define-method (raw (imm <mem>) (bits <integer>))
  (raw (pointer-address (get-memory imm)) bits))
(define-method (bits3 (reg <integer>)) (logand reg #b111))
(define-method (bits3 (reg <reg<>>)) (bits3 (get-code reg)))
(define-method (bit4 (reg <integer>)) (logand reg #b1))
(define-method (bit4 (reg <reg<>>)) (bit4 (ash (get-code reg) -3)))
(define* (opcode code #:optional reg)
  (list (logior code (bits3 (or reg 0)))))
(define (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash (bits3 reg/opcode) 3) (bits3 r/m))))
(define (REX W R X B)
  (let ((flags (logior
                 (ash (if (is-a? W <reg<64>>) 1 0) 3)
                 (ash (bit4 R) 2)
                 (ash (bit4 X) 1)
                 (bit4 B))))
    (if (zero? flags) '() (list (logior (ash #b0100 4) flags)))))
(define (SIB SS index r)
  (list (logior (ash SS 6) (ash index 3) (bits3 r))))
(define (NOP) '(#x90))
(define (RET) '(#xc3))
(define-method (MOV (r/m <reg<32>>) (r <reg<32>>))
  (append (REX r/m r 0 r/m) (opcode #x89) (ModR/M #b11 r r/m)))
(define-method (MOV (r/m <reg<64>>) (r <reg<64>>))
  (append (REX r/m r 0 r/m) (opcode #x89) (ModR/M #b11 r r/m)))
(define-method (MOV (r/m <address>) (r <reg<32>>))
  (append (REX r r 0 r/m) (opcode #x89) (ModR/M #b00 r r/m)))
(define-method (MOV (r/m <address>) (r <reg<64>>))
  (append (REX r r 0 r/m) (opcode #x89) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<32>>) (imm32 <integer>))
  (append (REX r 0 0 r) (opcode #xb8 r) (raw imm32 32)))
(define-method (MOV (r <reg<64>>) (imm64 <integer>))
  (append (REX r 0 0 r) (opcode #xb8 r) (raw imm64 64)))
(define-method (MOV (r <reg<64>>) (imm64 <mem>))
  (append (REX r 0 0 r) (opcode #xb8 r) (raw imm64 64)))
(define-method (MOV (r <reg<32>>) (r/m <address>))
  (append (REX r r 0 r/m) (opcode #x8b) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<32>>) (r/m <address>) (disp <integer>))
  (let ((sib (if (equal? r/m *RSP) (SIB #b00 #b100 r/m) '())))
    (append (REX r r 0 r/m) (opcode #x8b) (ModR/M #b01 r r/m) sib (raw disp 8))))
(define-method (SHL (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (opcode #xd1) (ModR/M #b11 4 r/m)))
(define-method (SHR (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (opcode #xd1) (ModR/M #b11 5 r/m)))
(define-method (SAL (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (opcode #xd1) (ModR/M #b11 4 r/m)))
(define-method (SAR (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (opcode #xd1) (ModR/M #b11 7 r/m)))
(define-method (ADD (r/m <reg<32>>) (r <reg<32>>))
  (append (REX r/m r 0 r/m) (opcode #x01) (ModR/M #b11 r r/m)))
(define-method (ADD (r/m <reg<64>>) (r <reg<64>>))
  (append (REX r/m r 0 r/m) (opcode #x01) (ModR/M #b11 r r/m)))
(define-method (ADD (r/m <reg<>>) (imm32 <integer>))
  (if (equal? (get-code r/m) 0)
    (append (REX r/m 0 0 r/m) (opcode #x05) (raw imm32 32))
    (append (REX r/m 0 0 r/m) (opcode #x81) (ModR/M #b11 0 r/m) (raw imm32 32))))
(define-method (PUSH (r <reg<64>>))
  (opcode #x50 r))
(define-method (POP (r <reg<64>>))
  (opcode #x58 r))
(define-method (JMP (rel32 <integer>))
  (append (opcode #xe9) (raw rel32 32)))
(define-method (NEG (r/m <reg<32>>))
  (append (REX r/m 0 0 r/m) (opcode #xf7) (ModR/M #b11 3 r/m)))
(define-method (NEG (r/m <reg<64>>))
  (append (REX r/m 0 0 r/m) (opcode #xf7) (ModR/M #b11 3 r/m)))
(define-method (SUB (r/m <reg<32>>) (r <reg<32>>))
  (append (REX r/m r 0 r/m) (opcode #x29) (ModR/M #b11 r r/m)))
(define-method (SUB (r/m <reg<64>>) (r <reg<64>>))
  (append (REX r/m r 0 r/m) (opcode #x29) (ModR/M #b11 r r/m)))
(define-method (SUB (r/m <reg<>>) (imm32 <integer>))
  (if (equal? (get-code r/m) 0)
    (append (REX r/m 0 0 r/m) (opcode #x2d) (raw imm32 32))
    (append (REX r/m 0 0 r/m) (opcode #x81) (ModR/M #b11 5 r/m) (raw imm32 32))))
(define-syntax-rule (label name) (list 'label (quote name)))
