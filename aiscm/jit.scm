(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  ;#:use-module (ice-9 binary-ports)
  #:export (<jit-context> <jit-function>
            get-code asm resolve-jumps get-bits ptr get-disp get-index
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
            reg loc arg pass-parameters)
  #:export-syntax (env jit-wrap))
; http://www.drpaulcarter.com/pcasm/
; http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
(load-extension "libguile-jit" "init_jit")
(define-class <jit-context> () (binaries #:init-value '()))

(define-class <jcc> ()
  (target #:init-keyword #:target #:getter get-target)
  (code #:init-keyword #:code #:getter get-code))
(define-method (len (self <list>)) (length self))
(define-method (len (self <symbol>)) 0)
(define-method (len (self <jcc>)) 2)
(define-method (Jcc (target <symbol>) (code <integer>))
  (make <jcc> #:target target #:code code))
(define-method (Jcc (target <integer>) (code <integer>))
  (append (list code) (raw target 8))); TODO: long jumps
(define-method (resolve-jump self offset offsets) self)
(define-method (resolve-jump (self <jcc>) offset offsets)
  (let [(target (assq-ref offsets (get-target self)))]
    (Jcc (- target offset) (get-code self))))
(define (resolve-jumps commands); TODO: iterate until offsets become stable
  (let* [(addresses (integral (map len commands)))
         (labels    (filter (compose symbol? car) (zipmap commands addresses)))
         (resolve   (lambda (cmd adr) (resolve-jump cmd adr labels)))]
    (filter (compose not symbol?) (map resolve commands addresses))))

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

(define (asm ctx return-type arg-types commands)
  (let* [(resolved    (resolve-jumps commands))
         (with-return (attach resolved (RET)))
         (code        (make-mmap (u8-list->bytevector (flatten with-return))))]
    ;(call-with-output-file "debug.obj" (lambda (f) (put-bytevector f (u8-list->bytevector (flatten with-return)))))
    ; objdump -D -b binary -Mintel -mi386:x86-64 debug.obj
    (slot-set! ctx 'binaries (cons code (slot-ref ctx 'binaries)))
    (pointer->procedure (foreign-type return-type)
                        (make-pointer (mmap-address code))
                        (map foreign-type arg-types))))

(define-class <operand> ())

(define-class <register> (<operand>)
  (bits #:init-keyword #:bits #:getter get-bits)
  (code #:init-keyword #:code #:getter get-code))

(define hex (upto #x0 #xf))
(define register-sizes '(1 2 4 8))
(define (each-hex proc arg) (for-each proc arg hex))
(define (reg-list bits) (map (cut make <register> #:bits bits #:code <>) hex))
(define regs (map (compose reg-list (cut * <> 8)) register-sizes))
(define-method (reg (type <meta<int<>>>) (code <integer>))
  (list-ref (list-ref regs (index (size-of type) register-sizes)) code))

(each-hex (lambda (sym val) (toplevel-define! sym (reg <byte> val)))
          '(AL CL DL BL SPL BPL SIL DIL R8L R9L R10L R11L R12L R13L R14L R15L))

(each-hex (lambda (sym val) (toplevel-define! sym (reg <sint> val)))
          '(AX CX DX BX SP BP SI DI R8W R9W R10W R11W R12W R13W R14W R15W))

(each-hex (lambda (sym val) (toplevel-define! sym (reg <int> val)))
          '(EAX ECX EDX EBX ESP EBP ESI EDI R8D R9D R10D R11D R12D R13D R14D R15D))

(each-hex (lambda (sym val) (toplevel-define! sym (reg <long> val)))
          '(RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15))

(define (scale s) (index s register-sizes))

(define-class <pointer> (<operand>)
  (type  #:init-keyword #:type  #:getter get-type)
  (reg   #:init-keyword #:reg   #:getter get-reg)
  (disp  #:init-keyword #:disp  #:getter get-disp  #:init-value #f)
  (index #:init-keyword #:index #:getter get-index #:init-value #f))

(define-method (get-bits (self <pointer>)) (* 8 (size-of (get-type self))))

(define-method (ptr (type <meta<int<>>>) (reg <register>))
  (make <pointer> #:type type #:reg reg))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (disp <integer>))
  (make <pointer> #:type type #:reg reg #:disp disp))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (index <register>))
  (make <pointer> #:type type #:reg reg #:index index))
(define-method (ptr (type <meta<int<>>>) (reg <register>) (index <register>) (disp <integer>))
  (make <pointer> #:type type #:reg reg #:index index #:disp disp))

(define-method (raw (imm <boolean>) (bits <integer>)) '())
(define-method (raw (imm <integer>) (bits <integer>))
  (bytevector->u8-list (pack (make (integer bits unsigned) #:value imm))))
(define-method (raw (imm <mem>) (bits <integer>))
  (raw (pointer-address (get-memory imm)) bits))

(define-method (bits3 (x <integer>)) (logand x #b111))
(define-method (bits3 (x <register>)) (bits3 (get-code x)))
(define-method (bits3 (x <pointer>)) (bits3 (get-reg x)))

(define-method (get-reg   (x <register>)) #f)
(define-method (get-index (x <register>)) #f)
(define-method (get-disp  (x <register>)) #f)

(define-method (bit4 (x <boolean>)) 0)
(define-method (bit4 (x <integer>)) (logand x #b1))
(define-method (bit4 (x <register>)) (bit4 (ash (get-code x) -3)))
(define-method (bit4 (x <pointer>)) (bit4 (get-reg x)))

(define-method (disp-value (x <register>)) #f)
(define-method (disp-value (x <pointer>))
  (or (get-disp x) (if (memv (get-reg x) (list RBP R13)) 0 #f)))

(define (opcode code reg) (list (logior code (bits3 reg))))
(define (if8 reg a b) (list (if (eqv? (get-bits reg) 8) a b)))
(define (opcode-if8 reg code1 code2) (opcode (car (if8 reg code1 code2)) reg))
(define-method (op16 (x <integer>)) (if (eqv? x 16) (list #x66) '()))
(define-method (op16 (x <operand>)) (op16 (get-bits x)))

(define-method (disp8? (disp <boolean>)) #f)
(define-method (disp8? (disp <integer>)) (and (>= disp -128) (< disp 128)))

(define-method (mod (r/m <boolean>)) #b00)
(define-method (mod (r/m <integer>)) (if (disp8? r/m) #b01 #b10))
(define-method (mod (r/m <register>)) #b11)
(define-method (mod (r/m <pointer>)) (mod (disp-value r/m)))

(define-method (ModR/M mod reg/opcode r/m)
  (list (logior (ash mod 6) (ash (bits3 reg/opcode) 3) (bits3 r/m))))
(define-method (ModR/M reg/opcode (r/m <register>))
  (ModR/M (mod r/m) reg/opcode r/m))
(define-method (ModR/M reg/opcode (r/m <pointer>))
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

(define-method (MOV (m <pointer>) (r <register>))
  (append (prefixes r m) (if8 r #x88 #x89) (postfixes r m)))
(define-method (MOV (r <register>) imm)
  (append (prefixes r) (opcode-if8 r #xb0 #xb8) (raw imm (get-bits r))))
(define-method (MOV (m <pointer>) imm)
  (append (prefixes m) (if8 m #xc6 #xc7) (postfixes 0 m) (raw imm (min 32 (get-bits m)))))
(define-method (MOV (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x8a #x8b) (postfixes r r/m)))

(define-method (MOVSX (r <register>) (r/m <operand>))
  (let* [(bits   (get-bits r/m))
         (opcode (cond ((eqv? bits  8) (list #x0f #xbe))
                       ((eqv? bits 16) (list #x0f #xbf))
                       ((eqv? bits 32) (list #x63))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (MOVZX (r <register>) (r/m <operand>))
  (let* [(bits   (get-bits r/m))
         (opcode (cond ((eqv? bits  8) (list #x0f #xb6))
                       ((eqv? bits 16) (list #x0f #xb7))))]
    (append (prefixes r r/m) opcode (postfixes r r/m))))

(define-method (LEA (r <register>) (m <pointer>))
  (append (prefixes r m) (list #x8d) (postfixes r m)))

(define-method (SHL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SHR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 5 r/m)))
(define-method (SAL (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 4 r/m)))
(define-method (SAR (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xd0 #xd1) (postfixes 7 r/m)))

(define-method (ADD (m <pointer>) (r <register>))
  (append (prefixes r m) (if8 m #x00 #x01) (postfixes r m)))
(define-method (ADD (r <register>) imm)
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x04 #x05) (raw imm (min 32 (get-bits r))))
    (next-method)))
(define-method (ADD (r/m <operand>) imm)
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 0 r/m) (raw imm (min 32 (get-bits r/m)))))
(define-method (ADD (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r #x02 #x03) (postfixes r r/m)))

(define-method (PUSH (r <register>)); TODO: PUSH r/m, PUSH imm
  (append (prefixes r) (opcode #x50 r)))
(define-method (POP (r <register>))
  (append (prefixes r) (opcode #x58 r)))

(define-method (NEG (r/m <operand>))
  (append (prefixes r/m) (if8 r/m #xf6 #xf7) (postfixes 3 r/m)))

(define-method (SUB (m <pointer>) (r <register>))
  (append (prefixes r m) (if8 m #x28 #x29) (postfixes r m)))
(define-method (SUB (r <register>) imm)
  (if (equal? (get-code r) 0)
    (append (prefixes r) (if8 r #x2c #x2d) (raw imm (min 32 (get-bits r))))
    (next-method)))
(define-method (SUB (r/m <operand>) imm)
  (append (prefixes r/m) (if8 r/m #x80 #x81) (postfixes 5 r/m) (raw imm (min 32 (get-bits r/m)))))
(define-method (SUB (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (if8 r/m #x2a #x2b) (postfixes r r/m)))

(define-method (IMUL (r <register>) (r/m <operand>))
  (append (prefixes r r/m) (list #x0f #xaf) (postfixes r r/m)))
(define-method (IMUL (r <register>) (r/m <operand>) (imm <integer>)); TODO: imm for more than 8 bit
  (append (prefixes r r/m) (list #x6b) (postfixes r r/m) (raw imm 8)))

(define-method (CMP (m <pointer>) (r <register>))
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
  (map get-code (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX RBP R12 R13 R14 R15)))
(define callee-saved-codes (map get-code (list RBX RSP RBP R12 R13 R14 R15)))
(define args (map get-code (list RDI RSI RDX RCX R8 R9)))
(define-class <jit-function> ()
  (codes #:init-value default-codes #:init-keyword #:codes #:getter get-codes)
  (live #:init-value '() #:init-keyword #:live #:getter get-live #:setter set-live)
  (before #:init-value '() #:getter get-before #:setter set-before)
  (after #:init-value '() #:getter get-after #:setter set-after)
  (offset #:init-value 0 #:init-keyword #:offset #:getter get-offset #:setter set-offset)
  (argc #:init-value 0 #:getter get-argc #:setter set-argc))
(define (get-free fun)
  (let [(live-codes (map get-code (get-live fun)))]
    (find (compose not (cut member <> live-codes)) (get-codes fun))))
(define (clear-before fun) (let [(retval (get-before fun))] (set-before fun '()) retval))
(define (clear-after fun) (let [(retval (get-after fun))] (set-after fun '()) retval))
(define (push-stack fun reg)
  (set-offset fun (1+ (get-offset fun)))
  (set-before fun (attach (get-before fun) (PUSH reg)))
  (set-after fun (cons (POP reg) (get-after fun))))
(define (spill fun type)
  (let* [(target (last (get-live fun)))
         (retval (reg type (get-code target)))]
    (push-stack fun target)
    (set-live fun (cons retval (all-but-last (get-live fun))))
    retval))
(define (allocate fun type)
  (let* [(code (get-free fun))
         (retval (if code (reg type code) #f))]
    (if retval (set-live fun (cons retval (get-live fun))))
    (if (member code callee-saved-codes) (push-stack fun (reg <long> code)))
    retval))
(define-method (reg (type <meta<int<>>>) (fun <jit-function>))
  (or (allocate fun type) (spill fun type)))
(define-method (arg (type <meta<int<>>>) (fun <jit-function>))
  (let* [(n       (get-argc fun))
         (is-reg? (< n (length args)))
         (value  (if is-reg?
                   (reg type (list-ref args n))
                   (ptr type RSP (ash (+ (- n (length args)) 1) 3))))]
    (if is-reg? (set-live fun (cons value (get-live fun))))
    (set-argc fun (1+ (get-argc fun)))
    (make type #:value value)))
(define-method (loc (value <register>) (fun <jit-function>)) value)
(define-method (loc (value <pointer>) (fun <jit-function>))
  (let [(disp (+ (get-disp value) (ash (get-offset fun) 3)))]
    (ptr (get-type value) RSP disp)))
(define-method (reg (value <register>) (fun <jit-function>))
  (set-live fun (cons value (delete value (get-live fun))))
  (loc value fun))
(define-method (reg (value <pointer>) (fun <jit-function>))
  (let* [(retval (reg (get-type value) fun))
         (setup  (MOV retval (loc value fun)))]
    (set-before fun (attach (get-before fun) setup))
    retval))
(define-syntax-rule (env fun vars . body)
  (let* [(live   (get-live fun))
         (before (clear-before fun))
         (after  (clear-after fun))
         (offset (get-offset fun))
         (middle (let vars (list . body)))
         (start  (get-before fun))
         (end    (get-after fun))]
    (set-live fun live)
    (set-before fun before)
    (set-after fun after)
    (set-offset fun offset)
    (append start (resolve-jumps (flatten-n middle 2)) end)))

(define-method (arg (type <meta<sequence<>>>) (fun <jit-function>))
  (let [(value   (get-value (arg <long> fun)))
        (shape   (expand (dimension type) (get-value (arg <long> fun))))
        (strides (expand (dimension type) (get-value (arg <long> fun))))]
    (make type #:value value #:shape shape #:strides strides)))

(define-method (types (type <meta<element>>)) type)
(define-method (types (type <meta<sequence<>>>))
  (cons <long> (append (expand (dimension type) <long>) (expand (dimension type) <long>))))
(define-method (content (self <element>)) (get-value self))
(define-method (content (self <sequence<>>))
  (cons ((compose pointer-address get-memory get-value) self)
        (append (shape self) (strides self))))
(define-method (return-type (type <meta<element>>)) type)
(define-method (return-type (type <meta<sequence<>>>)) <null>)
(define-method (add-return-value (type <meta<element>>) fun args)
  (cons (make type #:value (reg type fun)) args))
(define-method (add-return-value (type <meta<sequence<>>>) fun args) args)
(define-method (add-return-param (type <meta<element>>) arg-classes) arg-classes)
(define-method (add-return-param (type <meta<sequence<>>>) arg-classes) (cons type arg-classes))
(define (pass-parameters ctx return-class arg-classes expr)
  (let* [(fun           (make <jit-function>))
         (param-classes (add-return-param return-class arg-classes))
         (args          (map (cut arg <> fun) param-classes))
         (return-type   (return-type return-class))
         (arg-types     (flatten (map types param-classes)))
         (vals          (add-return-value return-class fun args))]
    (asm ctx return-type arg-types (apply expr (cons fun vals)))))
(define-method (shape a b)
  (let [(shape-a (shape a))
        (shape-b (shape b))]
    (if (>= (length shape-a) (length shape-b)) shape-a shape-b)))
(define-method (pass-return-value (ctx <jit-context>) (return-class <meta<element>>)
                                  arg-classes (fun <procedure>))
  (let* [(code (pass-parameters ctx return-class arg-classes fun))
         (proc (lambda args
                 (let [(result (apply code (flatten (map content args))))]
                   (make return-class #:value result))))]
    (make <method> #:specializers arg-classes #:procedure proc)))
(define-method (pass-return-value (ctx <jit-context>) (return-class <meta<sequence<>>>)
                                  arg-classes (fun <procedure>))
  (let* [(code (pass-parameters ctx return-class arg-classes fun))
         (proc (lambda args
                 (let [(retval (make return-class #:shape (apply shape args)))]
                   (apply code (flatten (map content (cons retval args))))
                   retval)))]
    (make <method> #:specializers arg-classes #:procedure proc)))

(define-syntax-rule (jit-wrap ctx return-class (arg-class ...) fun)
  (pass-return-value ctx return-class (list arg-class ...) fun))
