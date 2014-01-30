(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:export (jit-call
            make-mmap
            ADD
            JMP
            MOV
            NOP
            RET
            EAX
            ECX
            EDX
            EBX
            ESP
            EBP
            ESI
            EDI))
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
(define MOV_r32,imm32   #xb8)
(define-class <register> () (code #:init-keyword #:code #:getter get-code))
(define EAX (make <register> #:code 0))
(define ECX (make <register> #:code 1))
(define EDX (make <register> #:code 2))
(define EBX (make <register> #:code 3))
(define ESP (make <register> #:code 4))
(define EBP (make <register> #:code 5))
(define ESI (make <register> #:code 6))
(define EDI (make <register> #:code 7))
(define (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash reg/opcode 3) r/m)))
(define-method (ADD (r/m32 <register>) (r32 <register>))
  (let ((op ADD_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (ADD (r/m32 <register>) (imm32 <int>))
  (let ((id (bytevector->u8-list (pack imm32))))
    (if (equal? r/m32 EAX)
      (let ((op ADD_EAX,imm32))
        (append (list op) id))
      (let ((op ADD_r/m32,imm32)
            (r/m (get-code r/m32))
            (mod #b11))
        (append (list op) (ModR/M mod 0 r/m) id)))))
(define-method (JMP (rel32 <int>))
  (let ((op JMP_rel32)
        (cd (bytevector->u8-list (pack rel32))))
    (append (list op) cd)))
(define-method (MOV (r/m32 <register>) (r32 <register>))
  (let ((op MOV_r/m32,r32)
        (reg (get-code r32))
        (r/m (get-code r/m32))
        (mod #b11))
    (append (list op) (ModR/M mod reg r/m))))
(define-method (MOV (r32 <register>) (imm32 <int>))
  (let ((op  MOV_r32,imm32)
        (reg (get-code r32))
        (id  (bytevector->u8-list (pack imm32))))
    (append (list (logior op reg)) id)))
(define (NOP) '(#x90))
(define (RET) '(#xc3))
