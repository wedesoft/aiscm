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
            R8D
            R9D
            R10D
            R11D
            R12D
            R13D
            R14D
            R15D
            RAX
            RCX
            RDX
            RBX
            RSP
            RBP
            RSI
            RDI
            *RAX
            *RCX
            *RDX
            *RBX
            *???
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
(define ADD_r/m32,r32   #x01)
(define ADD_EAX,imm32   #x05)
(define ADD_RAX,imm32   #x05)
(define ADD_r/m32,imm32 #x81)
(define ADD_r/m64,imm32 #x81)
(define JMP_rel32       #xe9)
(define MOV_r/m32,r32   #x89)
(define MOV_r32,r/m32   #x8b)
(define MOV_r32,imm32   #xb8)
(define MOV_r64,imm64   #xb8)
(define PUSH_r32        #x50)
(define POP_r32         #x58)
(define SAL_r/m32,1     #xd1)
(define SAR_r/m32,1     #xd1)
(define SHL_r/m32,1     #xd1)
(define SHR_r/m32,1     #xd1)
(define-class <reg32> () (code #:init-keyword #:code #:getter get-code))
(define EAX (make <reg32> #:code  #b000))
(define ECX (make <reg32> #:code  #b001))
(define EDX (make <reg32> #:code  #b010))
(define EBX (make <reg32> #:code  #b011))
(define ESP (make <reg32> #:code  #b100))
(define EBP (make <reg32> #:code  #b101))
(define ESI (make <reg32> #:code  #b110))
(define EDI (make <reg32> #:code  #b111))
(define  R8D (make <reg32> #:code #b1000))
(define  R9D (make <reg32> #:code #b1001))
(define R10D (make <reg32> #:code #b1010))
(define R11D (make <reg32> #:code #b1011))
(define R12D (make <reg32> #:code #b1100))
(define R13D (make <reg32> #:code #b1101))
(define R14D (make <reg32> #:code #b1110))
(define R15D (make <reg32> #:code #b1111))
(define-class <reg64> () (code #:init-keyword #:code #:getter get-code))
(define RAX (make <reg64> #:code #b000))
(define RCX (make <reg64> #:code #b001))
(define RDX (make <reg64> #:code #b010))
(define RBX (make <reg64> #:code #b011))
(define RSP (make <reg64> #:code #b100))
(define RBP (make <reg64> #:code #b101))
(define RSI (make <reg64> #:code #b110))
(define RDI (make <reg64> #:code #b111))
(define-class <address> () (code #:init-keyword #:code #:getter get-code))
(define *RAX    (make <address> #:code #b000))
(define *RCX    (make <address> #:code #b001))
(define *RDX    (make <address> #:code #b010))
(define *RBX    (make <address> #:code #b011))
(define *???    (make <address> #:code #b100))
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
(define REX.W (REX 1 0 0 0))
(define (REX.B B) (REX 0 0 0 B))
(define (REX.RB R B) (REX 0 R 0 B))
(define-method (MOV (r/m32 <reg32>) (r32 <reg32>))
  (let* ((op  MOV_r/m32,r32)
         (reg (get-code r32))
         (r/m (get-code r/m32))
         (rex (REX.RB (ash reg -3) (ash r/m -3)))
         (mod #b11))
    (append rex (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r/m32 <address>) (r32 <reg32>))
  (let ((op  MOV_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b00))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r32 <reg32>) (imm32 <integer>))
  (let ((op  MOV_r32,imm32)
        (reg (get-code r32))
        (id  (raw imm32 32)))
    (append (REX.B (ash reg -3)) (list (logior op reg)) id)))
(define-method (MOV (r64 <reg64>) (imm64 <integer>))
  (let ((op  MOV_r64,imm64)
        (reg (get-code r64))
        (id  (raw imm64 64)))
    (append REX.W (list (logior op reg)) id)))
(define-method (MOV (r32 <reg32>) (imm32 <mem>))
  (MOV r32 (ptr->int imm32)))
(define-method (MOV (r64 <reg64>) (imm64 <mem>))
  (MOV r64 (ptr->int imm64)))
(define-method (MOV (r32 <reg32>) (r/m32 <address>))
  (let ((op  MOV_r32,r/m32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b00))
    (append (list op) (ModR/M mod reg r/m))))
(define (NOP) '(#x90))
(define (RET) '(#xc3))
(define-method (SHL (r/m32 <reg32>))
  (let ((op  SHL_r/m32,1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 4 r/m))))
(define-method (SHL (r/m64 <reg64>))
  (let ((op  SHL_r/m32,1)
        (r/m (get-code r/m64))
        (mod #b11))
        (append REX.W (list op) (ModR/M mod 4 r/m))))
(define-method (SHR (r/m32 <reg32>))
  (let ((op  SHR_r/m32,1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 5 r/m))))
(define-method (SHR (r/m64 <reg64>))
  (let ((op  SHR_r/m32,1)
        (r/m (get-code r/m64))
        (mod #b11))
        (append REX.W (list op) (ModR/M mod 5 r/m))))
(define-method (SAL (r/m32 <reg32>))
  (let ((op  SAL_r/m32,1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 4 r/m))))
(define-method (SAL (r/m64 <reg64>))
  (let ((op  SAL_r/m32,1)
        (r/m (get-code r/m64))
        (mod #b11))
        (append REX.W (list op) (ModR/M mod 4 r/m))))
(define-method (SAR (r/m32 <reg32>))
  (let ((op  SAR_r/m32,1)
        (r/m (get-code r/m32))
        (mod #b11))
        (append (list op) (ModR/M mod 7 r/m))))
(define-method (SAR (r/m64 <reg64>))
  (let ((op  SAR_r/m32,1)
        (r/m (get-code r/m64))
        (mod #b11))
        (append REX.W (list op) (ModR/M mod 7 r/m))))
(define-method (ADD (r/m32 <reg32>) (r32 <reg32>))
  (let ((op  ADD_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (ADD (r/m32 <reg32>) (imm32 <integer>))
  (let ((id (raw imm32 32)))
    (if (equal? r/m32 EAX)
      (let ((op ADD_EAX,imm32))
        (append (list op) id))
      (let ((op  ADD_r/m32,imm32)
            (r/m (get-code r/m32))
            (mod #b11))
        (append (list op) (ModR/M mod 0 r/m) id)))))
(define-method (ADD (r/m64 <reg64>) (imm32 <integer>))
  (let ((id (raw imm32 32)))
    (if (equal? r/m64 RAX)
      (let ((op ADD_RAX,imm32))
        (append REX.W (list op) id))
      (let ((op  ADD_r/m64,imm32)
            (r/m (get-code r/m64))
            (mod #b11))
        (append REX.W (list op) (ModR/M mod 0 r/m) id)))))
(define-method (PUSH (r32 <reg32>))
  (let ((op  PUSH_r32)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (POP (r32 <reg32>))
  (let ((op  POP_r32)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (JMP (rel32 <integer>))
  (let ((op JMP_rel32)
        (cd (raw rel32 32)))
    (append (list op) cd)))
