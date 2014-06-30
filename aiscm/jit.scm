(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:use-module (aiscm int)
  #:use-module (aiscm mem)
  #:export (<jit-context>
            <operand>   <meta<operand>>
            <addr<>>    <meta<addr<>>>
            <addr<8>>   <meta<addr<8>>>
            <addr<16>>  <meta<addr<16>>>
            <addr<32>>  <meta<addr<32>>>
            <addr<64>>  <meta<addr<64>>>
            <reg<>>     <meta<reg<>>>
            <reg<8>>    <meta<reg<8>>>
            <reg<16>>   <meta<reg<16>>>
            <reg<32>>   <meta<reg<32>>>
            <reg<64>>   <meta<reg<64>>>
            <jcc>
            <pool>
            get-reg get-name asm label-offsets get-target resolve resolve-jumps len get-bits
            byte-ptr word-ptr dword-ptr qword-ptr get-disp get-scale get-index
            ADD MOV MOVSX MOVZX LEA NOP RET PUSH POP SAL SAR SHL SHR NEG SUB CMP
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
            *1 *2 *4 *8
            reg)
  #:export-syntax (environment))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> ()
  (binaries #:init-value '()))

(define-class <jcc> ()
  (target #:init-keyword #:target #:getter get-target)
  (code #:init-keyword #:code #:getter get-code))
(define-method (len (self <jcc>)) 2)
(define-method (Jcc (target <symbol>) (code <integer>))
  (make <jcc> #:target target #:code code))
(define-method (Jcc (target <integer>) (code <integer>))
  (append (list code) (raw target 8)))
(define-method (resolve (self <jcc>) (offset <integer>) offsets)
  (let [(target (- (assq-ref offsets (get-target self)) offset))]
    (Jcc target (get-code self))))

(define (label-offsets commands)
  (define (iterate cmd acc)
    (let [(offsets (car acc))
          (offset  (cdr acc))]
      (if (is-a? cmd <symbol>)
        (cons (acons cmd offset offsets) offset)
        (let [(len-cmd (if (is-a? cmd <jcc>) (len cmd) (length cmd)))]
          (cons offsets (+ offset len-cmd))))))
  (car (fold iterate (cons '() 0) commands)))

(define (resolve-jumps commands offsets)
  (define (iterate cmd acc)
    (let [(tail   (car acc))
          (offset (cdr acc))]
      (cond
        ((is-a? cmd <jcc>)    (cons (cons (resolve cmd (+ offset (len cmd)) offsets) tail)
                                    (+ offset (len cmd))))
        ((is-a? cmd <symbol>) (cons tail offset))
        (else                 (cons (cons cmd tail) (+ offset (length cmd)))))))
  (reverse (car (fold iterate (cons '() 0) commands))))

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

(define (asm ctx return_type commands . args)
  (let* [(offsets  (label-offsets commands))
         (resolved (resolve-jumps commands offsets))
         (code     (make-mmap (u8-list->bytevector (apply append resolved))))]
    (slot-set! ctx 'binaries (cons code (slot-ref ctx 'binaries)))
    (pointer->procedure return_type (make-pointer (mmap-address code)) args)))

(define-class <meta<operand>> (<class>))
(define-class <operand> () #:metaclass <meta<operand>>)

(define-class <meta<reg<>>> (<meta<operand>>))
(define-class <reg<>> (<operand>)
              (code #:init-keyword #:code #:getter get-code)
              #:metaclass <meta<reg<>>>)

(define hex (upto #x0 #xf))
(define (each-hex proc arg) (for-each proc arg hex))
(define (reg-list type) (map (cut make type #:code <>) hex))

(define-class <meta<reg<8>>> (<meta<reg<>>>))
(define-method (get-bits (self <meta<reg<8>>>)) 8)
(define-class <reg<8>> (<reg<>>) #:metaclass <meta<reg<8>>>)
(define reg8-list (reg-list <reg<8>>))
(define-method (reg (type <meta<reg<8>>>) (code <integer>)) (list-ref reg8-list code))
(each-hex (lambda (sym val) (toplevel-define! sym (reg <reg<8>> val)))
          '(AL CL DL BL SPL BPL SIL DIL R8L R9L R10L R11L R12L R13L R14L R15L))

(define-class <meta<reg<16>>> (<meta<reg<>>>))
(define-method (get-bits (self <meta<reg<16>>>)) 16)
(define-class <reg<16>> (<reg<>>) #:metaclass <meta<reg<16>>>)
(define reg16-list (reg-list <reg<16>>))
(define-method (reg (type <meta<reg<16>>>) (code <integer>)) (list-ref reg16-list code))
(each-hex (lambda (sym val) (toplevel-define! sym (reg <reg<16>> val)))
          '(AX CX DX BX SP BP SI DI R8W R9W R10W R11W R12W R13W R14W R15W)) 

(define-class <meta<reg<32>>> (<meta<reg<>>>))
(define-method (get-bits (self <meta<reg<32>>>)) 32)
(define-class <reg<32>> (<reg<>>) #:metaclass <meta<reg<32>>>)
(define reg32-list (reg-list <reg<32>>))
(define-method (reg (type <meta<reg<32>>>) (code <integer>)) (list-ref reg32-list code))
(each-hex (lambda (sym val) (toplevel-define! sym (reg <reg<32>> val)))
          '(EAX ECX EDX EBX ESP EBP ESI EDI R8D R9D R10D R11D R12D R13D R14D R15D))

(define-class <meta<reg<64>>> (<meta<reg<>>>))
(define-method (get-bits (self <meta<reg<64>>>)) 64)
(define-class <reg<64>> (<reg<>>) #:metaclass <meta<reg<64>>>)
(define reg64-list (reg-list <reg<64>>))
(define-method (reg (type <meta<reg<64>>>) (code <integer>)) (list-ref reg64-list code))
(each-hex (lambda (sym val) (toplevel-define! sym (reg <reg<64>> val)))
          '(RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15))

(define *1 #b00)
(define *2 #b01)
(define *4 #b10)
(define *8 #b11)

(define-class <meta<addr<>>> (<meta<operand>>))
(define-class <addr<>> (<operand>)
              (reg #:init-keyword #:reg #:getter get-reg)
              (disp #:init-keyword #:disp #:init-form #f #:getter get-disp)
              (scale #:init-keyword #:scale #:init-form *1 #:getter get-scale)
              (index #:init-keyword #:index #:init-form #f #:getter get-index)
              #:metaclass <meta<addr<>>>)

(define-class <meta<addr<8>>> (<meta<addr<>>>))
(define-method (get-bits (self <meta<addr<8>>>)) 8)
(define-class <addr<8>> (<addr<>>) #:metaclass <meta<addr<8>>>)

(define-class <meta<addr<16>>> (<meta<addr<>>>))
(define-method (get-bits (self <meta<addr<16>>>)) 16)
(define-class <addr<16>> (<addr<>>) #:metaclass <meta<addr<16>>>)

(define-class <meta<addr<32>>> (<meta<addr<>>>))
(define-method (get-bits (self <meta<addr<32>>>)) 32)
(define-class <addr<32>> (<addr<>>) #:metaclass <meta<addr<32>>>)

(define-class <meta<addr<64>>> (<meta<addr<>>>))
(define-method (get-bits (self <meta<addr<64>>>)) 64)
(define-class <addr<64>> (<addr<>>) #:metaclass <meta<addr<64>>>)

(define-method (addr (type <meta<addr<>>>) (reg <reg<64>>))
  (make type #:reg reg))
(define-method (addr (type <meta<addr<>>>) (reg <reg<64>>) (disp <integer>))
  (make type #:reg reg #:disp disp))
(define-method (addr (type <meta<addr<>>>) (reg <reg<64>>) (index <reg<64>>) (scale <integer>))
  (make type #:reg reg #:index index #:scale scale))
(define-method (addr (type <meta<addr<>>>) (reg <reg<64>>) (index <reg<64>>) (scale <integer>) (disp <integer>))
  (make type #:reg reg #:index index #:scale scale #:disp disp))

(define (byte-ptr  . args) (apply addr (cons <addr<8>>  args)))
(define (word-ptr  . args) (apply addr (cons <addr<16>> args)))
(define (dword-ptr . args) (apply addr (cons <addr<32>> args)))
(define (qword-ptr . args) (apply addr (cons <addr<64>> args)))

(define-method (raw (imm <boolean>) (bits <integer>)) '())
(define-method (raw (imm <integer>) (bits <integer>))
  (bytevector->u8-list (pack (make (integer bits unsigned) #:value imm))))
(define-method (raw (imm <mem>) (bits <integer>))
  (raw (pointer-address (get-memory imm)) bits))

(define-method (bits3 (x <integer>)) (logand x #b111))
(define-method (bits3 (x <reg<>>)) (bits3 (get-code x)))
(define-method (bits3 (x <addr<>>)) (bits3 (get-reg x)))

(define-method (get-reg (x <reg<>>)) #f)
(define-method (get-index (x <reg<>>)) #f)
(define-method (get-disp (x <reg<>>)) #f)

(define-method (bit4 (x <boolean>)) 0)
(define-method (bit4 (x <integer>)) (logand x #b1))
(define-method (bit4 (x <reg<>>)) (bit4 (ash (get-code x) -3)))
(define-method (bit4 (x <addr<>>)) (bit4 (get-reg x)))

(define (opcode code reg) (list (logior code (bits3 reg))))
(define (if8 reg a b) (list (if (eqv? (get-bits (class-of reg)) 8) a b)))
(define (opcode-if8 reg code1 code2) (opcode (car (if8 reg code1 code2)) reg))
(define-method (op16 (x <integer>)) (if (eqv? x 16) (list #x66) '()))
(define-method (op16 (x <operand>)) (op16 (get-bits (class-of x))))

(define-method (disp8? (disp <boolean>)) 8)
(define-method (disp8? (disp <integer>)) (and (>= disp -128) (< disp 128)))

(define-method (mod (r/m <boolean>)) #b00)
(define-method (mod (r/m <integer>)) (if (disp8? r/m) #b01 #b10))
(define-method (mod (r/m <reg<>>)) #b11)
(define-method (mod (r/m <addr<>>)) (mod (get-disp r/m)))

(define-method (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash (bits3 reg/opcode) 3) (bits3 r/m))))
(define-method (ModR/M reg/opcode (r/m <reg<>>))
  (ModR/M (mod r/m) reg/opcode r/m))
(define-method (ModR/M reg/opcode (r/m <addr<>>))
  (if (get-index r/m)
    (ModR/M (mod r/m) reg/opcode #b100)
    (ModR/M (mod r/m) reg/opcode (get-reg r/m))))

(define (need-rex? r) (member r (list SPL BPL SIL DIL)))
(define (REX W r r/m)
  (let [(flags (logior (ash (if (eqv? (get-bits (class-of W)) 64) 1 0) 3)
                       (ash (bit4 r) 2)
                       (ash (bit4 (get-index r/m)) 1)
                       (bit4 r/m)))]
    (if (or (not (zero? flags)) (need-rex? r) (need-rex? (get-index r/m)) (need-rex? r/m))
      (list (logior (ash #b0100 4) flags)) '())))

(define (SIB r/m)
  (if (get-index r/m)
    (list (logior (ash (get-scale r/m) 6)
                  (ash (bits3 (get-index r/m)) 3)
                  (bits3 (get-reg r/m))))
    (if (equal? (get-reg r/m) RSP)
      (list #b00100100)
      '())))

(define-method (prefixes (r/m <operand>))
  (append (op16 r/m) (REX r/m 0 r/m)))
(define-method (prefixes (r <reg<>>) (r/m <operand>))
  (append (op16 r) (REX r r r/m)))

(define (postfixes reg/opcode r/m)
  (append (ModR/M reg/opcode r/m) (SIB r/m) (raw (get-disp r/m) (if (disp8? (get-disp r/m)) 8 32))))

(define (NOP) '(#x90))
(define (RET) '(#xc3))

(define-method (MOV (m <addr<>>) (r <reg<>>))
  (append (prefixes r m) (if8 r #x88 #x89) (postfixes r m)))
(define-method (MOV (r <reg<>>) imm)
  (append (prefixes r) (opcode-if8 r #xb0 #xb8) (raw imm (get-bits (class-of r)))))
(define-method (MOV (m <addr<>>) imm)
  (append (prefixes m) (if8 m #xc6 #xc7) (postfixes 0 m) (raw imm (min 32 (get-bits (class-of m))))))
(define-method (MOV (r <reg<>>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x8a #x8b) (postfixes r r/m)))

(define-method (MOVSX (r <reg<>>) (r/m <operand>))
  (let* [(bits   (get-bits (class-of r/m)))
         (opcode (cond ((eqv? bits  8) (list #x0f #xbe))
                       ((eqv? bits 16) (list #x0f #xbf))
                       ((eqv? bits 32) (list #x63))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (MOVZX (r <reg<>>) (r/m <operand>))
  (let* [(bits   (get-bits (class-of r/m)))
         (opcode (cond ((eqv? bits  8) (list #x0f #xb6))
                       ((eqv? bits 16) (list #x0f #xb7))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (LEA (r <reg<64>>) (m <addr<>>))
  (append (prefixes r m) (list #x8d) (postfixes r m)))

(define-method (SHL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SHR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 5 r/m)))
(define-method (SAL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SAR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 7 r/m)))

(define-method (ADD (m <addr<>>) (r <reg<>>))
  (append (prefixes r m) (if8 m #x00 #x01) (postfixes r m)))
(define-method (ADD (r <reg<>>) imm)
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x04 #x05) (raw imm (min 32 (get-bits (class-of r)))))
    (next-method)))
(define-method (ADD (r/m <operand>) imm)
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 0 r/m) (raw imm (min 32 (get-bits (class-of r/m))))))
(define-method (ADD (r <reg<>>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x02 #x03) (postfixes r r/m)))

(define-method (PUSH (r <reg<>>)); TODO: PUSH r/m, PUSH imm
  (append (prefixes r) (opcode #x50 r)))
(define-method (POP (r <reg<>>))
  (append (prefixes r) (opcode #x58 r)))

(define-method (NEG (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xf6 #xf7) (postfixes 3 r/m)))

(define-method (SUB (m <addr<>>) (r <reg<>>))
  (append (prefixes r m) (if8 m #x28 #x29) (postfixes r m)))
(define-method (SUB (r <reg<>>) imm)
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x2c #x2d) (raw imm (min 32 (get-bits (class-of r)))))
    (next-method)))
(define-method (SUB (r/m <operand>) imm)
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 5 r/m) (raw imm (min 32 (get-bits (class-of r/m))))))
(define-method (SUB (r <reg<>>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r/m #x2a #x2b) (postfixes r r/m)))

(define-method (CMP (m <addr<>>) (r <reg<>>))
  (append (prefixes r m) (if8 m #x38 #x39) (postfixes r m)))
(define-method (CMP (r <reg<>>) (imm <integer>))
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x3c #x3d) (raw imm (min 32 (get-bits (class-of r)))))
    (next-method)))
(define-method (CMP (r/m <operand>) (imm <integer>))
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 7 r/m) (raw imm (min 32 (get-bits (class-of r/m))))))
(define-method (CMP (r <reg<>>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r/m #x3a #x3b) (postfixes r r/m)))

(define (SETcc code r/m)
  (append (prefixes r/m) (list #x0f code) (postfixes 0 r/m)))
(define (SETB   r/m) (SETcc #x92 r/m))
(define (SETNB  r/m) (SETcc #x93 r/m))
(define (SETE   r/m) (SETcc #x94 r/m))
(define (SETNE  r/m) (SETcc #x95 r/m))
(define (SETBE  r/m) (SETcc #x96 r/m))
(define (SETNBE r/m) (SETcc #x97 r/m))
(define (SETL   r/m) (SETcc #x9c r/m))
(define (SETNL  r/m) (SETcc #x9d r/m))
(define (SETLE  r/m) (SETcc #x9e r/m))
(define (SETNLE r/m) (SETcc #x9f r/m))

(define default-codes
  (map get-code (list RAX RCX RDX RSI RDI RBX RBP R10 R11 R9 R8 R12 R13 R14 R15)))
(define callee-saved-codes
  (map get-code (list RBX RSP RBP R12 R13 R14 R15)))
(define-class <pool> ()
  (codes #:init-keyword #:codes #:init-default default-codes #:getter get-codes)
  (live #:init-value '() #:getter get-live #:setter set-live)
  (stack #:init-value '() #:getter get-stack #:setter set-stack))
(define-method (initialize (self <pool>) initargs)
  (let-keywords initargs #f (registers)
    (next-method self `(#:codes ,(map get-code registers)))))
(define (get-free pool)
  (let [(live-codes (map get-code (get-live pool)))]
    (find (compose not (cut member <> live-codes)) (get-codes pool))))
(define (clear-stack pool) (let [(retval (get-stack pool))] (set-stack pool '()) retval))
(define (push-stack pool reg) (set-stack pool (cons reg (get-stack pool))))
(define (spill pool type)
  (let* [(target (last (get-live pool)))
         (retval (reg type (get-code target)))]
    (push-stack pool target)
    (set-live pool (cons retval (take (get-live pool) (1- (length (get-live pool))))))
    retval))
(define (allocate pool type)
  (let* [(code (get-free pool))
         (retval (if code (reg type code) #f))]
    (if retval (set-live pool (cons retval (get-live pool))))
    (if (member code callee-saved-codes) (push-stack pool (reg <reg<64>> code)))
    retval))
(define-method (reg (type <class>) (pool <pool>))
  (or (allocate pool type) (spill pool type)))
(define-syntax-rule (environment pool vars . body)
  (let* [(live   (get-live pool))
         (stack  (clear-stack pool))
         (block  (let vars (list . body)))
         (pushes (map PUSH (reverse (get-stack pool))))
         (pops   (map POP (get-stack pool)))]
    (set-live pool live)
    (set-stack pool stack)
    (flatten-n (append pushes block pops) 2)))
