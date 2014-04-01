(define-module (aiscm jit)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:export (<jit-context>
            <operand> <meta<operand>>
            <reg<>>   <meta<reg<>>>
            <reg<8>>  <meta<reg<8>>>
            <reg<16>> <meta<reg<16>>>
            <reg<32>> <meta<reg<32>>>
            <reg<64>> <meta<reg<64>>>
            <jcc>
            get-name asm label-offsets get-target resolve resolve-jumps len regsize
            ADD MOV NOP RET PUSH POP SAL SAR SHL SHR NEG SUB CMP
            SETB SETNB SETE SETNE SETBE SETNBE SETL SETNL SETLE SETNLE
            JMP JB JNB JE JNE JBE JNBE JL JNL JLE JNLE
            AL CL DL BL SPL BPL SIL DIL
            R8L R9L R10L R11L R12L R13L R14L R15L
            AX CX DX BX SP BP SI DI
            R8W R9W R10W R11W R12W R13W R14W R15W
            EAX ECX EDX EBX ESP EBP ESI EDI
            R8D R9D R10D R11D R12D R13D R14D R15D
            RAX RCX RDX RBX RSP RBP RSI RDI
            R8 R9 R10 R11 R12 R13 R14 R15
            *RAX *RCX *RDX *RBX *RSP *disp32 *RSI *RDI
            *R8 *R9 *R10 *R11 *R12 *R13 *R14 *R15))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> ()
  (binaries #:init-value '()))
(define (label-offsets commands)
  (define (iterate cmd acc)
    (let ((offsets (car acc))
          (offset  (cdr acc)))
      (if (is-a? cmd <symbol>)
        (cons (acons cmd offset offsets) offset)
        (let ((len-cmd (if (is-a? cmd <jcc>) (len cmd) (length cmd))))
          (cons offsets (+ offset len-cmd))))))
  (car (fold iterate (cons '() 0) commands)))
(define-class <jcc> ()
  (target #:init-keyword #:target #:getter get-target)
  (code #:init-keyword #:code #:getter get-code))
(define-method (len (self <jcc>)) 2)
(define-method (Jcc (target <symbol>) (code <integer>))
  (make <jcc> #:target target #:code code))
(define-method (Jcc (target <integer>) (code <integer>))
  (append (opcode code) (raw target 8)))
(define-method (resolve (self <jcc>) (offset <integer>) offsets)
  (let ((target (- (assq-ref offsets (get-target self)) offset)))
    (Jcc target (get-code self))))
(define (JMP  target) (Jcc target #xeb))
(define (JB   target) (Jcc target #x72))
(define (JNB  target) (Jcc target #x73))
(define (JE   target) (Jcc target #x74))
(define (JNE  target) (Jcc target #x75))
(define (JBE  target) (Jcc target #x76))
(define (JNBE target) (Jcc target #x77))
(define (JL   target) (Jcc target #x7c))
(define (JNL  target) (Jcc target #x7d))
(define (JLE  target) (Jcc target #x7e))
(define (JNLE target) (Jcc target #x7f))
(define (resolve-jumps commands offsets)
  (define (iterate cmd acc)
    (let ((tail   (car acc))
          (offset (cdr acc)))
      (cond
        ((is-a? cmd <jcc>)    (cons (cons (resolve cmd (+ offset (len cmd)) offsets) tail)
                                    (+ offset (len cmd))))
        ((is-a? cmd <symbol>) (cons tail offset))
        (else                 (cons (cons cmd tail) (+ offset (length cmd)))))))
  (reverse (car (fold iterate (cons '() 0) commands))))
(define (asm ctx return_type commands . args)
  (let* ((offsets  (label-offsets commands))
         (resolved (resolve-jumps commands offsets))
         (code     (make-mmap (u8-list->bytevector (apply append resolved)))))
    (slot-set! ctx 'binaries (cons code (slot-ref ctx 'binaries)))
    (pointer->procedure return_type (make-pointer (mmap-address code)) args)))
(define-class <meta<operand>> (<class>))
(define-class <operand> ()
              (code #:init-keyword #:code #:getter get-code)
              #:metaclass <meta<operand>>)
(define-class <meta<reg<>>> (<meta<operand>>))
(define-class <reg<>> (<operand>) #:metaclass <meta<reg<>>>)
(define-class <meta<reg<8>>> (<meta<reg<>>>))
(define-method (regsize (self <meta<reg<8>>>)) 8)
(define-class <reg<8>> (<reg<>>) #:metaclass <meta<reg<8>>>)
(define   AL (make <reg<8>> #:code #b0000))
(define   CL (make <reg<8>> #:code #b0001))
(define   DL (make <reg<8>> #:code #b0010))
(define   BL (make <reg<8>> #:code #b0011))
(define  SPL (make <reg<8>> #:code #b0100))
(define  BPL (make <reg<8>> #:code #b0101))
(define  SIL (make <reg<8>> #:code #b0110))
(define  DIL (make <reg<8>> #:code #b0111))
(define  R8L (make <reg<8>> #:code #b1000))
(define  R9L (make <reg<8>> #:code #b1001))
(define R10L (make <reg<8>> #:code #b1010))
(define R11L (make <reg<8>> #:code #b1011))
(define R12L (make <reg<8>> #:code #b1100))
(define R13L (make <reg<8>> #:code #b1101))
(define R14L (make <reg<8>> #:code #b1110))
(define R15L (make <reg<8>> #:code #b1111))
(define-class <meta<reg<16>>> (<meta<reg<>>>))
(define-method (regsize (self <meta<reg<16>>>)) 16)
(define-class <reg<16>> (<reg<>>) #:metaclass <meta<reg<16>>>)
(define   AX (make <reg<16>> #:code #b0000))
(define   CX (make <reg<16>> #:code #b0001))
(define   DX (make <reg<16>> #:code #b0010))
(define   BX (make <reg<16>> #:code #b0011))
(define   SP (make <reg<16>> #:code #b0100))
(define   BP (make <reg<16>> #:code #b0101))
(define   SI (make <reg<16>> #:code #b0110))
(define   DI (make <reg<16>> #:code #b0111))
(define  R8W (make <reg<16>> #:code #b1000))
(define  R9W (make <reg<16>> #:code #b1001))
(define R10W (make <reg<16>> #:code #b1010))
(define R11W (make <reg<16>> #:code #b1011))
(define R12W (make <reg<16>> #:code #b1100))
(define R13W (make <reg<16>> #:code #b1101))
(define R14W (make <reg<16>> #:code #b1110))
(define R15W (make <reg<16>> #:code #b1111))
(define-class <meta<reg<32>>> (<meta<reg<>>>))
(define-method (regsize (self <meta<reg<32>>>)) 32)
(define-class <reg<32>> (<reg<>>) #:metaclass <meta<reg<32>>>)
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
(define-class <meta<reg<64>>> (<meta<reg<>>>))
(define-method (regsize (self <meta<reg<64>>>)) 64)
(define-class <reg<64>> (<reg<>>) #:metaclass <meta<reg<64>>>)
(define RAX (make <reg<64>> #:code #b0000))
(define RCX (make <reg<64>> #:code #b0001))
(define RDX (make <reg<64>> #:code #b0010))
(define RBX (make <reg<64>> #:code #b0011))
(define RSP (make <reg<64>> #:code #b0100))
(define RBP (make <reg<64>> #:code #b0101))
(define RSI (make <reg<64>> #:code #b0110))
(define RDI (make <reg<64>> #:code #b0111))
(define  R8 (make <reg<64>> #:code #b1000))
(define  R9 (make <reg<64>> #:code #b1001))
(define R10 (make <reg<64>> #:code #b1010))
(define R11 (make <reg<64>> #:code #b1011))
(define R12 (make <reg<64>> #:code #b1100))
(define R13 (make <reg<64>> #:code #b1101))
(define R14 (make <reg<64>> #:code #b1110))
(define R15 (make <reg<64>> #:code #b1111))
(define-class <meta<address>> (<meta<operand>>))
(define-method (regsize (self <meta<address>>)) 64)
(define-class <address> (<operand>) #:metaclass <meta<address>>)
(define *RAX    (make <address> #:code #b0000))
(define *RCX    (make <address> #:code #b0001))
(define *RDX    (make <address> #:code #b0010))
(define *RBX    (make <address> #:code #b0011))
(define *RSP    (make <address> #:code #b0100))
(define *disp32 (make <address> #:code #b0101))
(define *RSI    (make <address> #:code #b0110))
(define *RDI    (make <address> #:code #b0111))
(define  *R8    (make <address> #:code #b1000))
(define  *R9    (make <address> #:code #b1001))
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
(define-method (bits3 (reg <operand>)) (bits3 (get-code reg)))
(define-method (bit4 (reg <integer>)) (logand reg #b1))
(define-method (bit4 (reg <operand>)) (bit4 (ash (get-code reg) -3)))
(define* (opcode code #:optional reg)
  (list (logior code (bits3 (or reg 0)))))
(define op16 (list #x66))
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
(define-method (MOV (r/m <reg<8>>) (r <reg<8>>))
  (append (REX r/m r 0 r/m) (list #x88) (ModR/M #b11 r r/m)))
(define-method (MOV (r/m <reg<>>) (r <reg<>>))
  (append (REX r/m r 0 r/m) (list #x89) (ModR/M #b11 r r/m)))
(define-method (MOV (r/m <reg<16>>) (r <reg<16>>))
  (append op16 (REX r/m r 0 r/m) (list #x89) (ModR/M #b11 r r/m)))
(define-method (MOV (r/m <address>) (r <reg<>>))
  (append (REX r r 0 r/m) (list #x89) (ModR/M #b00 r r/m)))
(define-method (MOV (r/m <address>) (r <reg<8>>))
  (append (REX r r 0 r/m) (list #x88) (ModR/M #b00 r r/m)))
(define-method (MOV (r/m <address>) (r <reg<16>>))
  (append op16 (REX r r 0 r/m) (list #x89) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<8>>) imm)
  (append (REX r 0 0 r) (opcode #xb0 r) (raw imm (regsize (class-of r)))))
(define-method (MOV (r <reg<>>) imm)
  (append (REX r 0 0 r) (opcode #xb8 r) (raw imm (regsize (class-of r)))))
(define-method (MOV (r <reg<16>>) imm)
  (append op16 (REX r 0 0 r) (opcode #xb8 r) (raw imm (regsize (class-of r)))))
(define-method (MOV (r <reg<8>>) (r/m <address>))
  (append (REX r r 0 r/m) (list #x8a) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<>>) (r/m <address>))
  (append (REX r r 0 r/m) (list #x8b) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<16>>) (r/m <address>))
  (append op16 (REX r r 0 r/m) (list #x8b) (ModR/M #b00 r r/m)))
(define-method (MOV (r <reg<>>) (r/m <address>) (disp <integer>))
  (let ((sib (if (equal? r/m *RSP) (SIB #b00 #b100 r/m) '())))
    (append (REX r r 0 r/m) (list #x8b) (ModR/M #b01 r r/m) sib (raw disp 8))))
(define-method (SHL (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (list #xd1) (ModR/M #b11 4 r/m)))
(define-method (SHR (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (list #xd1) (ModR/M #b11 5 r/m)))
(define-method (SAL (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (list #xd1) (ModR/M #b11 4 r/m)))
(define-method (SAR (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (list #xd1) (ModR/M #b11 7 r/m)))
(define-method (ADD (r/m <reg<>>) (r <reg<>>))
  (append (REX r/m r 0 r/m) (list #x01) (ModR/M #b11 r r/m)))
(define-method (ADD (r/m <reg<>>) (imm32 <integer>))
  (if (equal? (get-code r/m) 0)
    (append (REX r/m 0 0 r/m) (list #x05) (raw imm32 32))
    (append (REX r/m 0 0 r/m) (list #x81) (ModR/M #b11 0 r/m) (raw imm32 32))))
(define-method (PUSH (r <reg<64>>))
  (opcode #x50 r))
(define-method (POP (r <reg<64>>))
  (opcode #x58 r))
(define-method (NEG (r/m <reg<>>))
  (append (REX r/m 0 0 r/m) (list #xf7) (ModR/M #b11 3 r/m)))
(define-method (SUB (r/m <reg<>>) (r <reg<>>))
  (append (REX r/m r 0 r/m) (list #x29) (ModR/M #b11 r r/m)))
(define-method (SUB (r/m <reg<>>) (imm32 <integer>))
  (if (equal? (get-code r/m) 0)
    (append (REX r/m 0 0 r/m) (list #x2d) (raw imm32 32))
    (append (REX r/m 0 0 r/m) (list #x81) (ModR/M #b11 5 r/m) (raw imm32 32))))
(define-method (CMP (r/m <reg<>>) (imm32 <integer>))
  (if (equal? (get-code r/m) 0)
    (append (REX r/m 0 0 r/m) (list #x3d) (raw imm32 32))
    (append (REX r/m 0 0 r/m) (list #x81) (ModR/M #b11 7 r/m) (raw imm32 32))))
(define-method (CMP (r/m <reg<>>) (r <reg<>>))
  (append (REX r r 0 r/m) (list #x39) (ModR/M #b11 r r/m)))
(define (SETcc code r/m)
  (append (REX 0 0 0 r/m) (list #x0f code) (opcode #xc0 r/m)))
(define-method (SETB   (r/m <reg<8>>)) (SETcc #x92 r/m))
(define-method (SETNB  (r/m <reg<8>>)) (SETcc #x93 r/m))
(define-method (SETE   (r/m <reg<8>>)) (SETcc #x94 r/m))
(define-method (SETNE  (r/m <reg<8>>)) (SETcc #x95 r/m))
(define-method (SETBE  (r/m <reg<8>>)) (SETcc #x96 r/m))
(define-method (SETNBE (r/m <reg<8>>)) (SETcc #x97 r/m))
(define-method (SETL   (r/m <reg<8>>)) (SETcc #x9c r/m))
(define-method (SETNL  (r/m <reg<8>>)) (SETcc #x9d r/m))
(define-method (SETLE  (r/m <reg<8>>)) (SETcc #x9e r/m))
(define-method (SETNLE (r/m <reg<8>>)) (SETcc #x9f r/m))
