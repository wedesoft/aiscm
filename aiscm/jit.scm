(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:use-module (aiscm mem)
  #:export (<jit-context>
            asm
            ADD
            JMP
            MOV
            NOP
            RET
            PUSH
            POP
            SAL
            SAR
            SHL
            SHR
            EAX
            ECX
            EDX
            EBX
            ESP
            EBP
            ESI
            EDI
            R8W
            R9W
            R10W
            R11W
            R12W
            R13W
            R14W
            R15W
            RAX
            RCX
            RDX
            RBX
            RSP
            RBP
            RSI
            RDI
            R8D
            R9D
            R10D
            R11D
            R12D
            R13D
            R14D
            R15D
            *RAX
            *RCX
            *RDX
            *RBX
            *RSP
            *disp32
            *RSI
            *RDI))
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
(define-class <reg32> () (code #:init-keyword #:code #:getter get-code))
(define  EAX (make <reg32> #:code #b0000))
(define  ECX (make <reg32> #:code #b0001))
(define  EDX (make <reg32> #:code #b0010))
(define  EBX (make <reg32> #:code #b0011))
(define  ESP (make <reg32> #:code #b0100))
(define  EBP (make <reg32> #:code #b0101))
(define  ESI (make <reg32> #:code #b0110))
(define  EDI (make <reg32> #:code #b0111))
(define  R8W (make <reg32> #:code #b1000))
(define  R9W (make <reg32> #:code #b1001))
(define R10W (make <reg32> #:code #b1010))
(define R11W (make <reg32> #:code #b1011))
(define R12W (make <reg32> #:code #b1100))
(define R13W (make <reg32> #:code #b1101))
(define R14W (make <reg32> #:code #b1110))
(define R15W (make <reg32> #:code #b1111))
(define-class <reg64> () (code #:init-keyword #:code #:getter get-code))
(define  RAX (make <reg64> #:code #b0000))
(define  RCX (make <reg64> #:code #b0001))
(define  RDX (make <reg64> #:code #b0010))
(define  RBX (make <reg64> #:code #b0011))
(define  RSP (make <reg64> #:code #b0100))
(define  RBP (make <reg64> #:code #b0101))
(define  RSI (make <reg64> #:code #b0110))
(define  RDI (make <reg64> #:code #b0111))
(define  R8D (make <reg64> #:code #b1000))
(define  R9D (make <reg64> #:code #b1001))
(define R10D (make <reg64> #:code #b1010))
(define R11D (make <reg64> #:code #b1011))
(define R12D (make <reg64> #:code #b1100))
(define R13D (make <reg64> #:code #b1101))
(define R14D (make <reg64> #:code #b1110))
(define R15D (make <reg64> #:code #b1111))
(define-class <address> () (code #:init-keyword #:code #:getter get-code))
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
(define (ModR/M mod reg/opcode r/m)
  (list (logior
          (ash (logand mod #b11) 6)
          (ash (logand reg/opcode #b111) 3)
          (logand r/m #b111))))
(define (REX W R X B)
  (let ((flags (logior
                 (ash (logand W 1) 3)
                 (ash (logand R 1) 2)
                 (ash (logand X 1) 1)
                 (logand B 1))))
    (if (zero? flags) '() (list (logior (ash #b0100 4) flags)))))
(define (SIB SS index r32)
  (list (logior (ash SS 6) (ash index 3) r32)))
(define-method (MOV (r/m32 <reg32>) (r32 <reg32>))
  (let* ((op  #x89)
         (reg (get-code r32))
         (r/m (get-code r/m32))
         (mod #b11)
         (rex (REX 0 (ash reg -3) 0 (ash r/m -3))))
    (append rex (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r/m64 <reg64>) (r64 <reg64>))
  (let* ((op  #x89)
         (reg (get-code r64))
         (r/m (get-code r/m64))
         (mod #b11)
         (rex (REX 1 (ash reg -3) 0 (ash r/m -3))))
    (append rex (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r/m32 <address>) (r32 <reg32>))
  (let ((op  #x89)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b00))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r32 <reg32>) (imm32 <integer>))
  (let* ((op  #xb8)
         (reg (get-code r32))
         (id  (raw imm32 32))
         (rex (REX 0 0 0 (ash reg -3))))
    (append rex (list (logior op reg)) id)))
(define-method (MOV (r64 <reg64>) (imm64 <integer>))
  (let ((op  #xb8)
        (reg (get-code r64))
        (id  (raw imm64 64))
        (rex (REX 1 0 0 0)))
    (append rex (list (logior op reg)) id)))
(define-method (MOV (r32 <reg32>) (imm32 <mem>))
  (MOV r32 (ptr->int imm32)))
(define-method (MOV (r64 <reg64>) (imm64 <mem>))
  (MOV r64 (ptr->int imm64)))
(define-method (MOV (r32 <reg32>) (r/m32 <address>))
  (let ((op  #x8b)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b00))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r32 <reg32>) (r/m32 <address>) (disp <integer>))
  (let* ((op  #x8b)
         (reg (get-code r32))
         (r/m (get-code r/m32))
         (mod #b01)
         (rex (REX 0 (ash reg -3) 0 (ash r/m -3)))
         (sib (if (eqv? r/m 4) (SIB #b00 #b100 r/m) '())))
    (append (list op) (ModR/M mod reg r/m) sib (raw disp 8))))
(define (NOP) '(#x90))
(define (RET) '(#xc3))
(define-method (SHL (r/m32 <reg32>))
  (let ((op  #xd1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 4 r/m))))
(define-method (SHL (r/m64 <reg64>))
  (let ((op  #xd1)
        (r/m (get-code r/m64))
        (mod #b11)
        (rex (REX 1 0 0 0)))
        (append rex (list op) (ModR/M mod 4 r/m))))
(define-method (SHR (r/m32 <reg32>))
  (let ((op  #xd1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 5 r/m))))
(define-method (SHR (r/m64 <reg64>))
  (let ((op  #xd1)
        (r/m (get-code r/m64))
        (mod #b11)
        (rex (REX 1 0 0 0)))
        (append rex (list op) (ModR/M mod 5 r/m))))
(define-method (SAL (r/m32 <reg32>))
  (let ((op  #xd1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 4 r/m))))
(define-method (SAL (r/m64 <reg64>))
  (let ((op  #xd1)
        (r/m (get-code r/m64))
        (mod #b11)
        (rex (REX 1 0 0 0)))
        (append rex (list op) (ModR/M mod 4 r/m))))
(define-method (SAR (r/m32 <reg32>))
  (let ((op  #xd1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 7 r/m))))
(define-method (SAR (r/m64 <reg64>))
  (let ((op  #xd1)
        (r/m (get-code r/m64))
        (mod #b11)
        (rex (REX 1 0 0 0)))
        (append rex (list op) (ModR/M mod 7 r/m))))
(define-method (ADD (r/m32 <reg32>) (r32 <reg32>))
  (let ((op  #x01)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (ADD (r/m32 <reg32>) (imm32 <integer>))
  (let ((id (raw imm32 32)))
    (if (equal? r/m32 EAX)
      (let ((op #x05))
        (append (list op) id))
      (let ((op  #x81)
            (r/m (get-code r/m32))
            (mod #b11))
        (append (list op) (ModR/M mod 0 r/m) id)))))
(define-method (ADD (r/m64 <reg64>) (imm32 <integer>))
  (let ((id (raw imm32 32)))
    (if (equal? r/m64 RAX)
      (let ((op #x05)
            (rex (REX 1 0 0 0)))
        (append rex (list op) id))
      (let ((op  #x81)
            (r/m (get-code r/m64))
            (mod #b11)
            (rex (REX 1 0 0 0)))
        (append rex (list op) (ModR/M mod 0 r/m) id)))))
(define-method (PUSH (r32 <reg32>))
  (let ((op  #x50)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (POP (r32 <reg32>))
  (let ((op  #x58)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (JMP (rel32 <integer>))
  (let ((op #xe9)
        (cd (raw rel32 32)))
    (append (list op) cd)))
