(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 binary-ports)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (<jit-context> <jit-function> <jcc> <cmd> <ptr> <operand> <register> <address> <var>
            asm obj resolve-jumps get-code get-bits ptr get-disp get-index get-target retarget
            ADD MOV MOVSX MOVZX LEA NOP RET PUSH POP SAL SAR SHL SHR NEG SUB IMUL CMP
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
            substitute-variables variables get-args input output labels next-indices live-analysis
            live-intervals overlap interference-graph callee-saved save-registers load-registers
            spill-variable save-and-use-registers register-allocate virtual-registers flatten-code relabel
            collate wrap idle-live fetch-parameters spill-parameters)
  #:export-syntax (env))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> () (binaries #:init-value '()))

(define-method (disp8? (disp <boolean>)) #f)
(define-method (disp8? (disp <integer>)) (and (>= disp -128) (< disp 128)))

(define-class <jcc> ()
  (target #:init-keyword #:target #:getter get-target)
  (code8 #:init-keyword #:code8 #:getter get-code8)
  (code32 #:init-keyword #:code32 #:getter get-code32))
(define-method (write (self <jcc>) port) (format port "(Jcc ~a)" (get-target self)))
(define-method (instruction-length self) 0)
(define-method (instruction-length (self <list>)) (length self))
(define-method (Jcc target code8 code32)
  (make <jcc> #:target target #:code8 code8 #:code32 code32))
(define-method (Jcc (target <integer>) code8 code32)
  (append (if (disp8? target) (list code8) code32) (raw target (if (disp8? target) 8 32))))
(define (retarget jcc target) (Jcc target (get-code8 jcc) (get-code32 jcc)))
(define-method (apply-offset self offsets) self)
(define-method (apply-offset (self <jcc>) offsets)
  (let [(pos    (assq-ref offsets self))
        (target (assq-ref offsets (get-target self)))]
    (retarget self (if target (- target pos) 0))))
(define (apply-offsets commands offsets) (map (cut apply-offset <> offsets) commands))
(define (stabilize-jumps commands guess)
  (let* [(applied  (apply-offsets commands guess))
         (sizes    (map instruction-length applied))
         (offsets  (map cons commands (integral sizes)))]
    (if (equal? offsets guess)
      (filter (compose not symbol?) applied)
      (stabilize-jumps commands offsets))))
(define (resolve-jumps commands) (stabilize-jumps commands '()))

(define (JMP  target) (Jcc target #xeb (list #xe9)))
(define (JB   target) (Jcc target #x72 (list #x0f #x82)))
(define (JNB  target) (Jcc target #x73 (list #x0f #x83)))
(define (JE   target) (Jcc target #x74 (list #x0f #x84)))
(define (JNE  target) (Jcc target #x75 (list #x0f #x85)))
(define (JBE  target) (Jcc target #x76 (list #x0f #x86)))
(define (JNBE target) (Jcc target #x77 (list #x0f #x87)))
(define (JL   target) (Jcc target #x7c (list #x0f #x8c)))
(define (JNL  target) (Jcc target #x7d (list #x0f #x8d)))
(define (JLE  target) (Jcc target #x7e (list #x0f #x8e)))
(define (JNLE target) (Jcc target #x7f (list #x0f #x8f)))

(define (obj commands) (u8-list->bytevector (flatten (resolve-jumps commands))))

(define (asm ctx return-type arg-types commands)
  (let* [(code   (obj commands))
         (mapped (make-mmap code))]
    ;(let [(filename (tmpnam))]
    ;  (call-with-output-file filename (cut put-bytevector <> code))
    ;  (system (format #f "objdump -D -b binary -Mintel -mi386:x86-64 ~a" filename)))
    (slot-set! ctx 'binaries (cons mapped (slot-ref ctx 'binaries)))
    (pointer->procedure (foreign-type return-type)
                        (make-pointer (mmap-address mapped))
                        (map foreign-type arg-types))))

(define-method (get-args self) '())
(define-method (input self) '())
(define-method (output self) '())
(define-class <cmd> ()
  (op #:init-keyword #:op #:getter get-op)
  (args #:init-keyword #:args #:getter get-args)
  (input #:init-keyword #:input #:getter get-input)
  (output #:init-keyword #:output #:getter get-output))
(define-method (initialize (self <cmd>) initargs)
  (let-keywords initargs #f (op (out '()) (io '()) (in '()))
    (next-method self (list #:op op
                            #:args (append out io in)
                            #:input (append io in)
                            #:output (append io out)))))

(define-method (input (self <cmd>))
  (delete-duplicates
    (filter is-var?
            (concatenate (cons (get-input self)
                               (map get-args
                                    (filter is-ptr? (get-args self))))))))
(define-method (output (self <cmd>)) (delete-duplicates (filter is-var? (get-output self))))
(define-method (write (self <cmd>) port)
  (write (cons (generic-function-name (get-op self)) (get-args self)) port))
(define-class <var> ()
  (type #:init-keyword #:type #:getter typecode)
  (symbol #:init-keyword #:symbol #:init-form (gensym)))
(define-method (write (self <var>) port) (write (slot-ref self 'symbol) port))
(define (is-var? value) (is-a? value <var>))
(define-class <ptr> ()
  (type #:init-keyword #:type #:getter typecode)
  (args #:init-keyword #:args #:getter get-args))
(define-method (write (self <ptr>) port)
  (write (cons 'ptr (cons (class-name (typecode self)) (get-args self))) port))
(define (is-ptr? value) (is-a? value <ptr>))
(define-method (substitute-variables self alist) self)
(define-method (substitute-variables (self <var>) alist)
  (let [(target (assq-ref alist self))]
    (if (is-a? target <register>)
      (reg (size-of (typecode self)) (get-code target)); TODO: do type conversion elsewhere
      (or target self))))
(define-method (substitute-variables (self <ptr>) alist)
  (apply ptr (cons (typecode self) (map (cut substitute-variables <> alist) (get-args self)))))
(define-method (substitute-variables (self <cmd>) alist)
  (apply (get-op self) (map (cut substitute-variables <> alist) (get-args self))))
(define-method (substitute-variables (self <list>) alist) (map (cut substitute-variables <> alist) self))

(define-syntax env
  (syntax-rules ()
    ((env [(name type) vars ...] body ...)
     (let [(name (make <var> #:type type #:symbol (quote name)))]
       (env [vars ...] body ...)))
    ((env [] body ...)
     (list body ...))))

(define-class <operand> ())

(define-class <register> (<operand>)
  (bits   #:init-keyword #:bits #:getter get-bits)
  (code   #:init-keyword #:code #:getter get-code)
  (symbol #:init-keyword #:symbol))
(define-method (write (self <register>) port) (format port "~a" (slot-ref self 'symbol)))

(define register-symbols
  '((1 AL  CL  DL  BL  SPL BPL SIL DIL R8L R9L R10L R11L R12L R13L R14L R15L)
    (2 AX  CX  DX  BX  SP  BP  SI  DI  R8W R9W R10W R11W R12W R13W R14W R15W)
    (4 EAX ECX EDX EBX ESP EBP ESI EDI R8D R9D R10D R11D R12D R13D R14D R15D)
    (8 RAX RCX RDX RBX RSP RBP RSI RDI R8  R9  R10  R11  R12  R13  R14  R15)))
(define (reg-list bytes lst)
  (map (lambda (sym code) (make <register> #:bits (ash bytes 3) #:code code #:symbol sym)) lst (iota #x10)))
(define regs (map (lambda (pair) (cons (car pair) (reg-list (car pair) (cdr pair)))) register-symbols))
(define (reg size code) (list-ref (assq-ref regs size) code))
(for-each
  (lambda (pair)
    (for-each
      (lambda (sym code) (toplevel-define! sym (reg (car pair) code))) (cdr pair) (iota #x10)))
  register-symbols)

(define (scale s) (index s '(1 2 4 8)))

(define-class <address> (<operand>)
  (type  #:init-keyword #:type  #:getter get-type)
  (reg   #:init-keyword #:reg   #:getter get-reg)
  (disp  #:init-keyword #:disp  #:getter get-disp  #:init-value #f)
  (index #:init-keyword #:index #:getter get-index #:init-value #f))
(define-method (write (self <address>) port)
  (format port "~a"
          (compact 'ptr (class-name (get-type self)) (get-reg self) (get-index self) (get-disp self))))

(define-method (get-bits (self <address>)) (* 8 (size-of (get-type self))))

(define-method (ptr (type <meta<int<>>>) . args)
  (make <ptr> #:type type #:args args))
(define-method (ptr (type <meta<int<>>>) (reg <register>))
  (make <address> #:type type #:reg reg))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (disp <integer>))
  (make <address> #:type type #:reg reg #:disp disp))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (index <register>))
  (make <address> #:type type #:reg reg #:index index))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (index <register>) (disp <integer>))
  (make <address> #:type type #:reg reg #:index index #:disp disp))

(define-method (raw (imm <boolean>) (bits <integer>)) '())
(define-method (raw (imm <integer>) (bits <integer>))
  (bytevector->u8-list (pack (make (integer bits unsigned) #:value imm))))
(define-method (raw (imm <mem>) (bits <integer>))
  (raw (pointer-address (get-memory imm)) bits))

(define-method (bits3 (x <integer>)) (logand x #b111))
(define-method (bits3 (x <register>)) (bits3 (get-code x)))
(define-method (bits3 (x <address>)) (bits3 (get-reg x)))

(define-method (get-reg   (x <register>)) #f)
(define-method (get-index (x <register>)) #f)
(define-method (get-disp  (x <register>)) #f)

(define-method (bit4 (x <boolean>)) 0)
(define-method (bit4 (x <integer>)) (logand x #b1))
(define-method (bit4 (x <register>)) (bit4 (ash (get-code x) -3)))
(define-method (bit4 (x <address>)) (bit4 (get-reg x)))

(define-method (disp-value (x <register>)) #f)
(define-method (disp-value (x <address>))
  (or (get-disp x) (if (memv (get-reg x) (list RBP R13)) 0 #f)))

(define (opcode code reg) (list (logior code (bits3 reg))))
(define (if8 reg a b) (list (if (eqv? (get-bits reg) 8) a b)))
(define (opcode-if8 reg code1 code2) (opcode (car (if8 reg code1 code2)) reg))
(define-method (op16 (x <integer>)) (if (eqv? x 16) (list #x66) '()))
(define-method (op16 (x <operand>)) (op16 (get-bits x)))

(define-method (mod (r/m <boolean>)) #b00)
(define-method (mod (r/m <integer>)) (if (disp8? r/m) #b01 #b10))
(define-method (mod (r/m <register>)) #b11)
(define-method (mod (r/m <address>)) (mod (disp-value r/m)))

(define-method (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash (bits3 reg/opcode) 3) (bits3 r/m))))
(define-method (ModR/M reg/opcode (r/m <register>))
  (ModR/M (mod r/m) reg/opcode r/m))
(define-method (ModR/M reg/opcode (r/m <address>))
  (if (get-index r/m)
    (ModR/M (mod r/m) reg/opcode #b100)
    (ModR/M (mod r/m) reg/opcode (get-reg r/m))))

(define (need-rex? r) (member r (list SPL BPL SIL DIL)))
(define (REX W r r/m)
  (let [(flags (logior (ash (if (eqv? (get-bits W) 64) 1 0) 3)
                       (ash (bit4 r) 2)
                       (ash (bit4 (get-index r/m)) 1)
                       (bit4 r/m)))]
    (if (or (not (zero? flags)) (need-rex? r) (need-rex? (get-index r/m)) (need-rex? r/m))
      (list (logior (ash #b0100 4) flags)) '())))

(define (SIB r/m)
  (if (get-index r/m)
    (list (logior (ash (scale (size-of (get-type r/m))) 6)
                  (ash (bits3 (get-index r/m)) 3)
                  (bits3 (get-reg r/m))))
    (if (memv (get-reg r/m) (list RSP R12))
      (list #b00100100)
      '())))

(define-method (prefixes (r/m <operand>))
  (append (op16 r/m) (REX r/m 0 r/m)))
(define-method (prefixes (r <register>) (r/m <operand>))
  (append (op16 r) (REX r r r/m)))

(define (postfixes reg/opcode r/m)
  (append (ModR/M reg/opcode r/m) (SIB r/m) (raw (disp-value r/m) (if (disp8? (disp-value r/m)) 8 32))))

(define (NOP) '(#x90))
(define (RET) '(#xc3))

(define-method (MOV arg1 arg2) (make <cmd> #:op MOV #:out (list arg1) #:in (list arg2)))
(define-method (MOV (m <address>) (r <register>))
  (append (prefixes r m) (if8 r #x88 #x89) (postfixes r m)))
(define-method (MOV (r <register>) (imm <integer>)); TODO: fix redundancy
  (append (prefixes r) (opcode-if8 r #xb0 #xb8) (raw imm (get-bits r))))
(define-method (MOV (m <address>) (imm <integer>)); TODO: fix redundancy
  (append (prefixes m) (if8 m #xc6 #xc7) (postfixes 0 m) (raw imm (min 32 (get-bits m)))))
(define-method (MOV (r <register>) (imm <mem>)); TODO: fix redundancy
  (append (prefixes r) (opcode-if8 r #xb0 #xb8) (raw imm (get-bits r))))
(define-method (MOV (m <address>) (imm <mem>)); TODO: fix redundancy
  (append (prefixes m) (if8 m #xc6 #xc7) (postfixes 0 m) (raw imm (min 32 (get-bits m)))))
(define-method (MOV (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x8a #x8b) (postfixes r r/m)))

(define-method (MOVSX arg1 arg2) (make <cmd> #:op MOVSX #:out (list arg1) #:in (list arg2)))
(define-method (MOVSX (r <register>) (r/m <operand>))
  (let* [(bits   (get-bits r/m))
         (opcode (case bits (( 8) (list #x0f #xbe))
                            ((16) (list #x0f #xbf))
                            ((32) (list #x63))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (MOVZX arg1 arg2) (make <cmd> #:op MOVZX #:out (list arg1) #:in (list arg2)))
(define-method (MOVZX (r <register>) (r/m <operand>))
  (let* [(bits   (get-bits r/m))
         (opcode (case bits (( 8) (list #x0f #xb6))
                            ((16) (list #x0f #xb7))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (LEA arg1 arg2) (make <cmd> #:op LEA #:out (list arg1) #:in (list arg2)))
(define-method (LEA (r <register>) (m <address>))
  (append (prefixes r m) (list #x8d) (postfixes r m)))

(define-method (SHL arg) (make <cmd> #:op SHL #:io (list arg)))
(define-method (SHL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SHR arg) (make <cmd> #:op SHR #:io (list arg)))
(define-method (SHR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 5 r/m)))
(define-method (SAL arg) (make <cmd> #:op SAL #:io (list arg)))
(define-method (SAL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SAR arg) (make <cmd> #:op SAR #:io (list arg)))
(define-method (SAR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 7 r/m)))

(define-method (ADD arg1 arg2) (make <cmd> #:op ADD #:io (list arg1) #:in (list arg2)))
(define-method (ADD (m <address>) (r <register>))
  (append (prefixes r m) (if8 m #x00 #x01) (postfixes r m)))
(define-method (ADD (r <register>) (imm <integer>))
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x04 #x05) (raw imm (min 32 (get-bits r))))
    (next-method)))
(define-method (ADD (r/m <operand>) (imm <integer>))
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 0 r/m) (raw imm (min 32 (get-bits r/m)))))
(define-method (ADD (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x02 #x03) (postfixes r r/m)))

(define-method (PUSH arg) (make <cmd> #:op PUSH #:in (list arg)))
(define-method (PUSH (r <register>)); TODO: PUSH r/m, PUSH imm
  (append (prefixes r) (opcode #x50 r)))
(define-method (POP arg) (make <cmd> #:op POP #:out (list arg)))
(define-method (POP (r <register>))
  (append (prefixes r) (opcode #x58 r)))

(define-method (NEG arg) (make <cmd> #:op NEG #:io (list arg)))
(define-method (NEG (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xf6 #xf7) (postfixes 3 r/m)))

(define-method (SUB arg1 arg2) (make <cmd> #:op SUB #:io (list arg1) #:in (list arg2)))
(define-method (SUB (m <address>) (r <register>))
  (append (prefixes r m) (if8 m #x28 #x29) (postfixes r m)))
(define-method (SUB (r <register>) (imm <integer>))
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x2c #x2d) (raw imm (min 32 (get-bits r))))
    (next-method)))
(define-method (SUB (r/m <operand>) (imm <integer>))
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 5 r/m) (raw imm (min 32 (get-bits r/m)))))
(define-method (SUB (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r/m #x2a #x2b) (postfixes r r/m)))

(define-method (IMUL arg1 . args) (make <cmd> #:op IMUL #:io (list arg1) #:in args))
(define-method (IMUL (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (list #x0f #xaf) (postfixes r r/m)))
(define-method (IMUL (r <register>) (r/m <operand>) (imm <integer>)); TODO: imm for more than 8 bit
  (append (prefixes r r/m) (list #x6b) (postfixes r r/m) (raw imm 8)))

(define-method (CMP arg1 arg2) (make <cmd> #:op CMP #:in (list arg1 arg2)))
(define-method (CMP (m <address>) (r <register>))
  (append (prefixes r m) (if8 m #x38 #x39) (postfixes r m)))
(define-method (CMP (r <register>) (imm <integer>))
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x3c #x3d) (raw imm (min 32 (get-bits r))))
    (next-method)))
(define-method (CMP (r/m <operand>) (imm <integer>))
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 7 r/m) (raw imm (min 32 (get-bits r/m)))))
(define-method (CMP (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r/m #x3a #x3b) (postfixes r r/m)))

(define (SETcc code r/m)
  (append (prefixes r/m) (list #x0f code) (postfixes 0 r/m)))
(define-method (SETB   arg) (make <cmd> #:op SETB   #:out (list arg)))
(define-method (SETB   (r/m <operand>)) (SETcc #x92 r/m))
(define-method (SETNB  arg) (make <cmd> #:op SETNB  #:out (list arg)))
(define-method (SETNB  (r/m <operand>)) (SETcc #x93 r/m))
(define-method (SETE   arg) (make <cmd> #:op SETE   #:out (list arg)))
(define-method (SETE   (r/m <operand>)) (SETcc #x94 r/m))
(define-method (SETNE  arg) (make <cmd> #:op SETNE  #:out (list arg)))
(define-method (SETNE  (r/m <operand>)) (SETcc #x95 r/m))
(define-method (SETBE  arg) (make <cmd> #:op SETBE  #:out (list arg)))
(define-method (SETBE  (r/m <operand>)) (SETcc #x96 r/m))
(define-method (SETNBE arg) (make <cmd> #:op SETNBE #:out (list arg)))
(define-method (SETNBE (r/m <operand>)) (SETcc #x97 r/m))
(define-method (SETL   arg) (make <cmd> #:op SETL   #:out (list arg)))
(define-method (SETL   (r/m <operand>)) (SETcc #x9c r/m))
(define-method (SETNL  arg) (make <cmd> #:op SETNL  #:out (list arg)))
(define-method (SETNL  (r/m <operand>)) (SETcc #x9d r/m))
(define-method (SETLE  arg) (make <cmd> #:op SETLE  #:out (list arg)))
(define-method (SETLE  (r/m <operand>)) (SETcc #x9e r/m))
(define-method (SETNLE arg) (make <cmd> #:op SETNLE #:out (list arg)))
(define-method (SETNLE (r/m <operand>)) (SETcc #x9f r/m))

(define (variables prog) (delete-duplicates (filter is-var? (concatenate (map get-args prog)))))
(define (labels prog) (filter (compose symbol? car) (map cons prog (iota (length prog)))))
(define-method (next-indices cmd k labels) (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices (cmd <jcc>) k labels)
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (eq? #xeb (get-code8 cmd)) (list target) (list (1+ k) target))))
(define (live-analysis prog)
  (letrec* [(inputs    (map input prog))
            (outputs   (map output prog))
            (indices   (iota (length prog)))
            (lut       (labels prog))
            (flow      (map (lambda (cmd k) (next-indices cmd k lut)) prog indices))
            (same?     (cut every (cut lset= equal? <...>) <...>))
            (track     (lambda (value)
                         (lambda (in ind out)
                           (union in (difference (apply union (map (cut list-ref value <>) ind)) out)))))
            (initial   (map (const '()) prog))
            (iteration (lambda (value) (map (track value) inputs flow outputs)))]
    (map union (fixed-point initial iteration same?) outputs)))
(define (live-intervals live variables)
  (map
    (lambda (v) (cons v (cons (first-index (cut memv v <>) live) (last-index (cut memv v <>) live))))
    variables))
(define ((overlap intervals) var)
  (let [(interval (assq-ref intervals var))]
    (map car (filter (lambda (x) (and (>= (cddr x) (car interval))
                                      (<= (cadr x) (cdr interval)))) intervals))))
(define (interference-graph live) (delete-duplicates (concatenate (map product live live))))
(define default-registers (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX RBP R12 R13 R14 R15))
(define (callee-saved registers)
  (lset-intersection eq? (delete-duplicates registers) (list RBX RSP RBP R12 R13 R14 R15)))
(define (save-registers registers offset)
  (map (lambda (register offset) (MOV (ptr <long> RSP offset) register))
       registers (iota (length registers) offset -8)))
(define (load-registers registers offset)
  (map (lambda (register offset) (MOV register (ptr <long> RSP offset)))
       registers (iota (length registers) offset -8)))
(define (relabel prog)
  (let* [(labels       (filter symbol? prog))
         (replacements (map (compose gensym symbol->string) labels))
         (translations (map cons labels replacements))]
    (map (lambda (x)
           (cond
             ((symbol? x)     (assq-ref translations x))
             ((is-a? x <jcc>) (retarget x (assq-ref translations (get-target x))))
             ((list? x)       (relabel x))
             (else            x)))
         prog)))
(define (flatten-code prog)
  (concatenate (map (lambda (x)
                      (if (and (list? x) (not (every integer? x)))
                        (flatten-code x)
                        (list x))) prog)))
(define ((insert-temporary var) cmd)
  (let [(temporary (make <var> #:type (typecode var)))]
    (compact
      (and (memv var (input cmd)) (MOV temporary var))
      (substitute-variables cmd (list (cons var temporary)))
      (and (memv var (output cmd)) (MOV var temporary)))))
(define (insert-temporaries var prog)
  (concatenate (map (insert-temporary var) prog)))
(define (spill-variable var location prog)
  (substitute-variables
    (insert-temporaries var prog)
    (list (cons var location))))
(define ((idle-live prog live) var)
  (count (lambda (cmd active) (and (not (memv var (get-args cmd))) (memv var active))) prog live))
(define ((spill-parameters parameters) colors)
  (filter-map (lambda (parameter register)
    (let [(value (assq-ref colors parameter))]; TODO: do type conversion elsewhere
      (if (is-a? value <address>) (MOV value (reg (size-of (typecode parameter)) (get-code register))) #f)))
    parameters (list RDI RSI RDX RCX R8 R9)))
(define ((fetch-parameters parameters) colors)
  (filter-map (lambda (parameter offset)
    (let [(value (assq-ref colors parameter))]
      (if (is-a? value <register>) (MOV (reg (size-of (typecode parameter)) (get-code value))
                                        (ptr (typecode parameter) RSP offset)) #f)))
    parameters (iota (length parameters) 8 8)))
(define (save-and-use-registers prog colors parameters offset)
  (let [(need-saving (callee-saved (map cdr colors)))]
    (append (save-registers need-saving offset)
            ((spill-parameters (take-up-to parameters 6)) colors)
            ((fetch-parameters (drop-up-to parameters 6)) colors)
            (all-but-last (substitute-variables prog colors))
            (load-registers need-saving offset)
            (list (RET)))))
(define* (register-allocate prog #:key (predefined '()) (registers default-registers) (parameters '()) (offset -8))
  (let* [(live       (live-analysis prog))
         (all-vars   (variables prog))
         (vars       (difference (variables prog) (map car predefined)))
         (conflicts  (live-intervals live all-vars))
         (colors     (color-intervals overlap assq-remove conflicts vars registers #:predefined predefined))
         (unassigned (find (compose not cdr) (reverse colors)))]
    (if unassigned
      (let* [(participants ((overlap conflicts) (car unassigned)))
             (var          (argmax (idle-live prog live) participants))
             (stack-param? (and (index var parameters) (<= 6 (index var parameters))))
             (location     (if stack-param?
                             (ptr (typecode var) RSP (* 8 (- (index var parameters) 5)))
                            (ptr (typecode var) RSP offset)))]
        (register-allocate (spill-variable var location prog)
                           #:predefined (assq-set predefined var location)
                           #:registers registers
                           #:parameters parameters
                           #:offset (if stack-param? offset (- offset 8))))
      (save-and-use-registers prog colors parameters offset))))
(define* (virtual-registers result-type arg-types proc #:key (registers default-registers))
  (let* [(result-types (if (eq? result-type <null>) '() (list result-type)))
         (arg-vars     (map (cut make <var> #:type <>) arg-types))
         (result-vars  (map (cut make <var> #:type <>) result-types))
         (arg-regs     (map cons arg-vars (list RDI RSI RDX RCX R8 R9)))
         (result-regs  (map cons result-vars (list RAX)))
         (vars         (append result-vars arg-vars))
         (predefined   (append result-regs arg-regs))
         (prog         (flatten-code (relabel (append '() (apply proc vars)))))]
    (register-allocate prog #:predefined predefined #:registers registers #:parameters arg-vars)))
(define (collate classes vars)
  (map param classes (gather (map (compose length types) classes) vars)))
(define (wrap ctx result-type arg-classes proc)
  (let* [(arg-types    (concatenate (map types arg-classes)))
         (result-types (if (eq? result-type <null>) '() (list result-type)))
         (code         (asm ctx result-type arg-types
                         (virtual-registers result-type arg-types
                           (lambda args (apply proc (collate (append result-types arg-classes) args))))))]
    (lambda params (apply code (concatenate (map content params))))))
