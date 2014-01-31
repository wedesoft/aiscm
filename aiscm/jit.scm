(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:use-module (aiscm pointer)
  #:export (jit-call
            make-mmap
            ADD
            JMP
            MOV
            NOP
            RET
            PUSH
            POP
            EAX
            ECX
            EDX
            EBX
            ESP
            EBP
            ESI
            EDI
            *EAX
            *ECX
            *EDX
            *EBX
            *???
            *disp32
            *ESI
            *EDI))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define (jit-call commands)
  (mmap-call (make-mmap (u8-list->bytevector (apply append commands)))))
(define ADD_r/m32,r32   #x01)
(define ADD_EAX,imm32   #x05)
(define ADD_r/m32,imm32 #x81)
(define JMP_rel32       #xe9)
(define MOV_r/m32,r32   #x89)
(define MOV_r32,r/m32   #x8b)
(define MOV_r32,imm32   #xb8)
(define PUSH_r32        #x50)
(define POP_r32         #x58)
(define-class <register> () (code #:init-keyword #:code #:getter get-code))
(define EAX (make <register> #:code #b000))
(define ECX (make <register> #:code #b001))
(define EDX (make <register> #:code #b010))
(define EBX (make <register> #:code #b011))
(define ESP (make <register> #:code #b100))
(define EBP (make <register> #:code #b101))
(define ESI (make <register> #:code #b110))
(define EDI (make <register> #:code #b111))
(define-class <address> () (code #:init-keyword #:code #:getter get-code))
(define *EAX    (make <address> #:code #b000))
(define *ECX    (make <address> #:code #b001))
(define *EDX    (make <address> #:code #b010))
(define *EBX    (make <address> #:code #b011))
(define *???    (make <address> #:code #b100))
(define *disp32 (make <address> #:code #b101))
(define *ESI    (make <address> #:code #b110))
(define *EDI    (make <address> #:code #b111))
(define-method (raw (imm32 <int>))
  (bytevector->u8-list (pack imm32)))
(define-method (raw (m32 <pointer<>>))
  (bytevector->u8-list (pack m32)))
(define (ptr->int ptr)
  (make <int> #:value (pointer-address (get-memory (get-value ptr)))))
(define (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash reg/opcode 3) r/m)))
(define-method (MOV (r/m32 <register>) (r32 <register>))
  (let ((op  MOV_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r32 <register>) (imm32 <int>))
  (let ((op  MOV_r32,imm32)
        (reg (get-code r32))
        (id  (raw imm32)))
    (append (list (logior op reg)) id)))
(define-method (MOV (r32 <register>) (imm32 <pointer<>>))
  (MOV r32 (ptr->int imm32)))
(define-method (MOV (r32 <register>) (r/m32 <address>))
  (let ((op  MOV_r32,r/m32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b00))
    (append (list op) (ModR/M mod reg r/m))))
(define (NOP) '(#x90))
(define (RET) '(#xc3))
(define-method (ADD (r/m32 <register>) (r32 <register>))
  (let ((op  ADD_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (ADD (r/m32 <register>) (imm32 <int>))
  (let ((id (raw imm32)))
    (if (equal? r/m32 EAX)
      (let ((op ADD_EAX,imm32))
        (append (list op) id))
      (let ((op  ADD_r/m32,imm32)
            (r/m (get-code r/m32))
            (mod #b11))
        (append (list op) (ModR/M mod 0 r/m) id)))))
(define-method (PUSH (r32 <register>))
  (let ((op  PUSH_r32)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (POP (r32 <register>))
  (let ((op  POP_r32)
        (reg (get-code r32)))
    (list (logior op reg))))
(define-method (JMP (rel32 <int>))
  (let ((op JMP_rel32)
        (cd (raw rel32)))
    (append (list op) cd)))
