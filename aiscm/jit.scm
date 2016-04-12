(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:export (<block> <cmd> <var> <ptr> <param> <tensor> <lookup> <function>
            ;<pointer<rgb<>>> <meta<pointer<rgb<>>>>
            ;<pointer<complex<>>> <meta<pointer<complex<>>>>
            substitute-variables variables get-args input output labels next-indices live-analysis
            callee-saved save-registers load-registers blocked repeat mov-signed mov-unsigned
            spill-variable save-and-use-registers register-allocate spill-blocked-predefines
            virtual-variables flatten-code relabel idle-live fetch-parameters spill-parameters
            filter-blocks blocked-intervals var skeleton parameter term tensor index type subst code
            assemble jit iterator step setup increment body arguments to-type operand
            duplicate shl shr sign-extend-ax div mod test-zero cmp-type ensure-default-strides
            unary-mutating unary-functional unary-extract composite-op)
  #:export-syntax (define-unary-op unary-fun))

(define ctx (make <context>))

(define-method (get-args self) '())
(define-method (input self) '())
(define-method (output self) '())
(define-class <cmd> ()
  (op     #:init-keyword #:op     #:getter get-op)
  (args   #:init-keyword #:args   #:getter get-args)
  (input  #:init-keyword #:input  #:getter get-input)
  (output #:init-keyword #:output #:getter get-output))
(define-method (initialize (self <cmd>) initargs)
  (let-keywords initargs #f (op (out '()) (io '()) (in '()))
    (next-method self (list #:op     op
                            #:args   (append out io in)
                            #:input  (append io in)
                            #:output (append io out)))))
(define-method (write (self <cmd>) port)
  (write (cons (generic-function-name (get-op self)) (get-args self)) port))
(define-method (equal? (a <cmd>) (b <cmd>))
  (and (eq? (get-op a) (get-op b)) (equal? (get-args a) (get-args b))))

(define-syntax-rule (mutating-op op)
  (define-method (op . args) (make <cmd> #:op op #:io (list (car args)) #:in (cdr args))))
(define-syntax-rule (functional-op op)
  (define-method (op . args) (make <cmd> #:op op #:out (list (car args)) #:in (cdr args))))
(define-syntax-rule (state-setting-op op)
  (define-method (op . args) (make <cmd> #:op op #:in args)))
(define-syntax-rule (state-reading-op op)
  (define-method (op . args) (make <cmd> #:op op #:out args)))

(define (mov-part a b) (MOV a (reg (size-of a) (get-code b))))
(define (mov-cmd movxx movxx32 a b)
  (cond
        ((eqv? (size-of a) (size-of b)) MOV)
        ((<    (size-of a) (size-of b)) mov-part)
        ((eqv? (size-of b) 4)           movxx32)
        (else                           movxx)))
(define-method (mov-signed   (a <operand>) (b <operand>)) ((mov-cmd MOVSX MOVSX a b) a b))
(define-method (mov-unsigned (a <operand>) (b <operand>)) ((mov-cmd MOVZX MOV   a b) a b))
(define-method (cmovnle16 (r <register>) (r/m <operand>))
  (CMOVNLE (reg (max (size-of r  ) 2) (get-code r  ))
           (reg (max (size-of r/m) 2) (get-code r/m))))
(define-method (cmovnbe16 (r <register>) (r/m <operand>))
  (CMOVNBE (reg (max (size-of r  ) 2) (get-code r  ))
           (reg (max (size-of r/m) 2) (get-code r/m))))
(define-method (cmovl16 (r <register>) (r/m <operand>))
  (CMOVL (reg (max (size-of r  ) 2) (get-code r  ))
         (reg (max (size-of r/m) 2) (get-code r/m))))
(define-method (cmovb16 (r <register>) (r/m <operand>))
  (CMOVB (reg (max (size-of r  ) 2) (get-code r  ))
         (reg (max (size-of r/m) 2) (get-code r/m))))

(functional-op    mov-signed)
(functional-op    mov-unsigned)
(mutating-op      cmovnle16)
(mutating-op      cmovnbe16)
(mutating-op      cmovl16)
(mutating-op      cmovb16)
(functional-op    MOV)
(functional-op    MOVSX)
(functional-op    MOVZX)
(functional-op    LEA)
(mutating-op      SHL)
(mutating-op      SHR)
(mutating-op      SAL)
(mutating-op      SAR)
(state-setting-op PUSH)
(state-reading-op POP)
(mutating-op      INC)
(mutating-op      IDIV)
(mutating-op      DIV)
(mutating-op      AND)
(mutating-op      OR)
(mutating-op      XOR)
(state-setting-op CMP)
(state-setting-op TEST)
(state-reading-op SETB)
(state-reading-op SETNB)
(state-reading-op SETE)
(state-reading-op SETNE)
(state-reading-op SETBE)
(state-reading-op SETNBE)
(state-reading-op SETL)
(state-reading-op SETNL)
(state-reading-op SETLE)
(state-reading-op SETNLE)
(mutating-op      CMOVB)
(mutating-op      CMOVNB)
(mutating-op      CMOVE)
(mutating-op      CMOVNE)
(mutating-op      CMOVBE)
(mutating-op      CMOVNBE)
(mutating-op      CMOVL)
(mutating-op      CMOVNL)
(mutating-op      CMOVLE)
(mutating-op      CMOVNLE)

(define-class <var> ()
  (type   #:init-keyword #:type   #:getter typecode)
  (symbol #:init-keyword #:symbol #:init-form (gensym)))
(define-method (write (self <var>) port) (write (slot-ref self 'symbol) port))
(define-method (size-of (self <var>)) (size-of (typecode self)))
(define-class <ptr> ()
  (type #:init-keyword #:type #:getter typecode)
  (args #:init-keyword #:args #:getter get-args))
(define-method (write (self <ptr>) port)
  (display (cons 'ptr (cons (class-name (typecode self)) (get-args self))) port))
(define-method (equal? (a <ptr>) (b <ptr>))
  (and (eq? (typecode a) (typecode b)) (equal? (get-args a) (get-args b))))
(define-method (ptr (type <meta<element>>) . args) (make <ptr> #:type type #:args args))
(define-method (variables self) '())
(define-method (variables (self <var>)) (list self))
(define-method (variables (self <cmd>)) (variables (get-args self)))
(define-method (variables (self <ptr>)) (variables (get-args self)))
(define-method (variables (self <list>)) (delete-duplicates (concatenate (map variables self))))
(define-method (input (self <cmd>))
  (delete-duplicates (variables (append (get-input self) (filter (cut is-a? <> <ptr>) (get-args self))))))
(define-method (output (self <cmd>)) (variables (get-output self)))
(define-method (substitute-variables self alist) self)
(define-method (substitute-variables (self <var>) alist)
  (let [(target (assq-ref alist self))]
    (if (is-a? target <register>)
      (reg (size-of self) (get-code target))
      (or target self))))
(define-method (substitute-variables (self <ptr>) alist)
  (apply ptr (cons (typecode self) (map (cut substitute-variables <> alist) (get-args self)))))
(define-method (substitute-variables (self <cmd>) alist)
  (apply (get-op self) (map (cut substitute-variables <> alist) (get-args self))))
(define-method (substitute-variables (self <list>) alist) (map (cut substitute-variables <> alist) self))

(define-method (var (self <meta<element>>)) (make <var> #:type self))
(define-method (var (self <meta<bool>>)) (var <ubyte>))
(define-method (var (self <meta<pointer<>>>)) (var <long>))
;(define-method (var (self <meta<complex<>>>)) (let [(t (base self))]
;  (make <internalcomplex> #:real-part (var t) #:imag-part (var t))))

(define (labels prog) (filter (compose symbol? car) (map cons prog (iota (length prog)))))
(define-method (next-indices cmd k labels) (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices (cmd <jcc>) k labels)
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (conditional? cmd) (list (1+ k) target) (list target))))
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
  (let [(instruction? (lambda (x) (and (list? x) (not (every integer? x)))))]
    (concatenate (map-if instruction? flatten-code list prog))))

(define ((insert-temporary target) cmd)
  (let [(temporary (var (typecode target)))]
    (compact
      (and (memv target (input cmd)) (MOV temporary target))
      (substitute-variables cmd (list (cons target temporary)))
      (and (memv target (output cmd)) (MOV target temporary)))))
(define (spill-variable var location prog)
  (substitute-variables (map (insert-temporary var) prog) (list (cons var location))))

(define ((idle-live prog live) var)
  (count (lambda (cmd active) (and (not (memv var (variables cmd))) (memv var active))) prog live))
(define ((spill-parameters parameters) colors)
  (filter-map (lambda (parameter register)
    (let [(value (assq-ref colors parameter))]
      (if (is-a? value <address>) (MOV value (reg (size-of parameter) (get-code register))) #f)))
    parameters (list RDI RSI RDX RCX R8 R9)))
(define ((fetch-parameters parameters) colors)
  (filter-map (lambda (parameter offset)
    (let [(value (assq-ref colors parameter))]
      (if (is-a? value <register>) (MOV (reg (size-of parameter) (get-code value))
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

(define (with-spilled-variable var location prog predefined blocked fun)
  (let* [(spill-code (spill-variable var location prog))]
    (fun (flatten-code spill-code)
         (assq-set predefined var location)
         (update-intervals blocked (index-groups spill-code)))))

(define* (register-allocate prog
                            #:key (predefined '())
                                  (blocked '())
                                  (registers default-registers)
                                  (parameters '())
                                  (offset -8))
  (let* [(live       (live-analysis prog))
         (all-vars   (variables prog))
         (vars       (difference (variables prog) (map car predefined)))
         (intervals  (live-intervals live all-vars))
         (adjacent   (overlap intervals))
         (colors     (color-intervals intervals
                                      vars
                                      registers
                                      #:predefined predefined
                                      #:blocked blocked))
         (unassigned (find (compose not cdr) (reverse colors)))]
    (if unassigned
      (let* [(target       (argmax (idle-live prog live) (adjacent (car unassigned))))
             (stack-param? (and (index-of target parameters) (>= (index-of target parameters) 6)))
             (location     (if stack-param?
                               (ptr (typecode target) RSP (* 8 (- (index-of target parameters) 5)))
                               (ptr (typecode target) RSP offset)))]
        (with-spilled-variable target location prog predefined blocked
          (lambda (prog predefined blocked)
            (register-allocate prog
                               #:predefined predefined
                               #:blocked blocked
                               #:registers registers
                               #:parameters parameters
                               #:offset (if stack-param? offset (- offset 8))))))
      (save-and-use-registers prog colors parameters offset))))

(define (blocked-predefined blocked predefined)
  (find (lambda (x) (memv (cdr x) (map car blocked))) predefined))

(define (spill-blocked-predefines prog . args)
  (let-keywords args #f [(predefined '())
                         (blocked '())
                         (registers default-registers)
                         (parameters '())
                         (offset -8)]
    (let [(conflict (blocked-predefined blocked predefined))]
      (if conflict
        (let* [(target   (car conflict))
               (location (ptr (typecode target) RSP offset))]
        (with-spilled-variable target location prog predefined blocked
          (lambda (prog predefined blocked)
            (spill-blocked-predefines prog
                                      #:predefined predefined
                                      #:blocked blocked
                                      #:registers registers
                                      #:parameters parameters
                                      #:offset (- offset 8)))))
      (apply register-allocate (cons prog args))))))

(define* (virtual-variables result-vars arg-vars intermediate #:key (registers default-registers))
  (let* [(result-regs  (map cons result-vars (list RAX)))
         (arg-regs     (map cons arg-vars (list RDI RSI RDX RCX R8 R9)))]
    (spill-blocked-predefines (flatten-code (relabel (filter-blocks intermediate)))
                              #:predefined (append result-regs arg-regs)
                              #:blocked (blocked-intervals intermediate)
                              #:registers registers
                              #:parameters arg-vars)))

(define (repeat n . body)
  (let [(i (var (typecode n)))]
    (list (MOV i 0) 'begin (CMP i n) (JE 'end) (INC i) body (JMP 'begin) 'end)))

(define-class <block> ()
  (reg  #:init-keyword #:reg  #:getter get-reg)
  (code #:init-keyword #:code #:getter get-code))
(define (blocked reg . body)
  (make <block> #:reg reg #:code body))
(define (filter-blocks prog)
  (cond
    ((is-a? prog <block>) (filter-blocks (get-code prog)))
    ((list? prog)         (map filter-blocks prog))
    (else                 prog)))
(define ((bump-interval offset) interval)
  (cons (car interval) (cons (+ (cadr interval) offset) (+ (cddr interval) offset))))
(define code-length (compose length flatten-code filter-blocks))
(define (blocked-intervals prog)
  (cond
    ((is-a? prog <block>) (cons (cons (get-reg prog) (cons 0 (1- (code-length (get-code prog)))))
                            (blocked-intervals (get-code prog))))
    ((pair? prog) (append (blocked-intervals (car prog))
                    (map (bump-interval (code-length (list (car prog))))
                         (blocked-intervals (cdr prog)))))
    (else '())))

(define (sign-extend-ax size) (case size ((1) (CBW)) ((2) (CWD)) ((4) (CDQ)) ((8) (CQO))))
(define (div/mod-prepare-signed r a) (list (MOV (reg r 0) a) (sign-extend-ax (size-of r))))
(define (div/mod-prepare-unsigned r a) (if (eqv? 1 (size-of r)) (list (MOVZX AX a)) (list (MOV (reg r 0) a) (MOV (reg r 2) 0))))
(define (div/mod-signed r a b) (attach (div/mod-prepare-signed r a) (IDIV b)))
(define (div/mod-unsigned r a b) (attach (div/mod-prepare-unsigned r a) (DIV b)))
(define (div/mod-block-registers r . code) (blocked RAX (if (eqv? 1 (size-of r)) code (blocked RDX code))))
(define (div/mod r a b . finalise) (div/mod-block-registers r ((if (signed? r) div/mod-signed div/mod-unsigned) r a b) finalise))
(define (div r a b) (div/mod r a b (MOV r (reg r 0))))
(define (mod r a b) (div/mod r a b (if (eqv? 1 (size-of r)) (list (MOV AL AH) (MOV r AL)) (MOV r DX))))

;(define ((binary-cmov op1 op2) r a b)
;  (if (= (size-of r) 1)
;    (list (CMP a b) (MOV r a) ((if (signed? (typecode r)) op1 op2) r b))
;    (list (CMP a b) (MOV r a) ((if (signed? (typecode r)) op1 op2) r b))))
;(define (sign-space a b)
;  (let [(coerced (coerce a b))]
;    (if (eqv? (signed? (typecode a)) (signed? (typecode b)))
;      coerced
;      (to-type (integer (min 64 (* 2 (bits (typecode coerced)))) signed) coerced))))
(define-method (mov a b); TODO: remove comparison with <bool>
  (list ((if (or (eq? (typecode b) <bool>) (signed? (typecode b))) mov-signed mov-unsigned) a b)))

(define-method (signed? (x <var>)) (signed? (typecode x)))
(define-method (signed? (x <ptr>)) (signed? (typecode x)))
(define (shx r x shift-signed shift-unsigned)
  (blocked RCX (mov-unsigned CL x) ((if (signed? r) shift-signed shift-unsigned) r CL)))
(define (shl r x) (shx r x SAL SHL))
(define (shr r x) (shx r x SAR SHR))
(define-method (test (a <var>)) (list (TEST a a)))
(define-method (test (a <ptr>))
  (let [(intermediate (var (typecode a)))]
    (list (MOV intermediate a) (test intermediate))))
(define (test-zero r a) (attach (test a) (SETE r)))
(define (test-non-zero r a) (attach (test a) (SETNE r)))
(define ((binary-bool op) a b)
  (let [(intermediate (var <byte>))]
    (attach (append (test-non-zero a a) (test-non-zero intermediate b)) (op a intermediate))))
(define bool-and (binary-bool AND))
(define bool-or  (binary-bool OR))

(define-method (cmp a b) (list (CMP a b)))
(define-method (cmp (a <ptr>) (b <ptr>))
  (let [(intermediate (var (typecode a)))]
    (cons (MOV intermediate a) (cmp intermediate b))))
(define (cmp-type a b)
  (if (eq? (signed? a) (signed? b))
      (coerce a b)
      (integer (min 64 (* 2 (bits (coerce a b)))) signed)))
(define (cmp-any a b)
  (let* [(type (cmp-type (typecode a) (typecode b)))
         (tmp1 (var type))
         (tmp2 (var type))]
    (append (if (eq? type (typecode a)) '() (mov tmp1 a)); TODO: refactor
            (if (eq? type (typecode b)) '() (mov tmp2 b))
            (cmp (if (eq? type (typecode a)) a tmp1)
                 (if (eq? type (typecode b)) b tmp2)))))
(define ((cmp-setxx set-signed set-unsigned) out a b)
  (let [(set (if (or (signed? a) (signed? b)) set-signed set-unsigned))]
    (attach (cmp-any a b) (set out))))
(define cmp-equal         (cmp-setxx SETE   SETE  ))
(define cmp-not-equal     (cmp-setxx SETNE  SETNE ))
(define cmp-lower-than    (cmp-setxx SETL   SETB  ))
(define cmp-lower-equal   (cmp-setxx SETLE  SETBE ))
(define cmp-greater-than  (cmp-setxx SETNLE SETNBE))
(define cmp-greater-equal (cmp-setxx SETNL  SETNB ))

(define-method (to-type (target <meta<element>>) (self <meta<element>>))
  target)
(define-method (to-type (target <meta<element>>) (self <meta<sequence<>>>))
  (multiarray target (dimensions self)))

(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<sequence<>>>))
  (let [(slice (skeleton (project self)))]
    (make self
          #:value   (value slice)
          #:shape   (cons (var <long>) (shape   slice))
          #:strides (cons (var <long>) (strides slice)))))

(define-class <param> ()
  (term #:init-keyword #:term #:getter term))

(define-class <tensor> (<param>)
  (dimension #:init-keyword #:dimension #:getter dimension)
  (index     #:init-keyword #:index     #:getter index))
(define (tensor dimension index term)
  (make <tensor> #:dimension dimension #:index index #:term term))

(define-class <lookup> (<param>)
  (index    #:init-keyword #:index    #:getter index)
  (stride   #:init-keyword #:stride   #:getter stride)
  (iterator #:init-keyword #:iterator #:getter iterator)
  (step     #:init-keyword #:step     #:getter step))
(define-method (lookup index term stride iterator step)
  (make <lookup> #:index index #:term term #:stride stride #:iterator iterator #:step step))
(define-method (lookup idx (obj <tensor>) stride iterator step)
  (tensor (dimension obj) (index obj) (lookup idx (term obj) stride iterator step)))

(define-class <function> (<param>)
  (arguments #:init-keyword #:arguments #:getter arguments)
  (type      #:init-keyword #:type      #:getter type)
  (project   #:init-keyword #:project   #:getter project))

(define-method (type (self <param>)) (typecode (term self)))
(define-method (type (self <tensor>)) (sequence (type (term self))))
(define-method (type (self <lookup>)) (type (term self)))
(define-method (typecode (self <tensor>)) (typecode (type self)))
(define-method (shape (self <tensor>)) (attach (shape (term self)) (dimension self))); TODO: get correct shape
(define-method (stride (self <tensor>)) (stride (term self))); TODO: get correct stride
(define-method (iterator (self <tensor>)) (iterator (term self))); TODO: get correct iterator
(define-method (step (self <tensor>)) (step (term self))); TODO: get correct step
(define-method (parameter (self <element>)) (make <param> #:term self))
(define-method (parameter (self <sequence<>>))
  (let [(idx (var <long>))]
    (tensor (dimension self)
            idx
            (lookup idx (parameter (project self)) (stride self) (var <long>) (var <long>)))))
(define-method (subst self candidate replacement) self)
(define-method (subst (self <tensor>) candidate replacement)
  (tensor (dimension self) (index self) (subst (term self) candidate replacement)))
(define-method (subst (self <lookup>) candidate replacement)
  (lookup (if (eq? (index self) candidate) replacement (index self))
          (subst (term self) candidate replacement)
          (stride self)
          (iterator self)
          (step self)))
(define-method (value (self <param>)) (value (term self)))
(define-method (value (self <tensor>)) (value (term self)))
(define-method (value (self <lookup>)) (value (term self)))
(define-method (rebase value (self <param>)) (parameter (rebase value (term self))))
(define-method (rebase value (self <tensor>))
  (tensor (dimension self) (index self) (rebase value (term self))))
(define-method (rebase value (self <lookup>))
  (lookup (index self) (rebase value (term self)) (stride self) (iterator self) (step self)))
(define-method (project (self <tensor>)) (project (term self) (index self)))
(define-method (project (self <tensor>) (idx <var>))
  (tensor (dimension self) (index self) (project (term self) idx)))
(define-method (project (self <lookup>) (idx <var>))
  (if (eq? (index self) idx)
      (term self)
      (lookup (index self) (project (term self)) (stride self) (iterator self) (step self))))
(define-method (get (self <tensor>) idx) (subst (term self) (index self) idx))

(define-method (setup self) '())
(define-method (setup (self <tensor>))
  (list (IMUL (step self) (stride self) (size-of (typecode self)))
        (MOV (iterator self) (value self))))
(define-method (setup (self <function>)) (concatenate (map setup (arguments self))))
(define-method (increment self) '())
(define-method (increment (self <tensor>)) (list (ADD (iterator self) (step self))))
(define-method (increment (self <function>)) (concatenate (map increment (arguments self))))
(define-method (body self) self)
(define-method (body (self <tensor>)) (project (rebase (iterator self) self)))
(define-method (body (self <function>)) ((project self)))

(define-method (operand (a <element>)) (get a))
(define-method (operand (a <pointer<>>))
  (if (pointer-offset a)
      (ptr (typecode a) (get a) (pointer-offset a))
      (ptr (typecode a) (get a))))

(define-syntax-rule (intermediate-var type intermediate body ...)
  (let [(intermediate (skeleton type))] (append body ...)))

(define-method (code (a <element>) (b <element>)) (mov (operand a) (operand b)))
(define-method (code (a <pointer<>>) (b <pointer<>>))
  (intermediate-var (typecode a) intermediate (code intermediate b) (code a intermediate)))
(define-method (code (a <param>) (b <param>)) (code (term a) (term b)))
(define-method (code (a <tensor>) (b <param>))
  (list (setup a)
        (setup b)
        (repeat (dimension a)
                (append (code (body a) (body b))
                        (increment a)
                        (increment b)))))
(define-method (code (out <element>) (fun <function>)) ((term fun) (parameter out)))
(define-method (code (out <pointer<>>) (fun <function>))
  (intermediate-var (typecode out) intermediate (code intermediate fun) (code out intermediate)))
(define-method (code (out <param>) (fun <function>))
  (code (term out) fun))

(define-method (composite-op (a <param>)) (lambda (out) (composite-op (type out) out a)))
(define-method (composite-op (a <param>) (b <param>)) (lambda (out) (composite-op (type out) out a b)))

(define-method (composite-op (t <meta<int<>>>) out a) (unary-mutating NEG out a))
(define-method (composite-op (t <meta<int<>>>) out a b) (binary-mutating SUB out a b))

(define (make-function name conversion kind cmd . args)
  (make <function> #:arguments args
                   #:type (apply conversion (map type args))
                   #:project (lambda ()  (apply name (map body args)))
                   #:term
                     (if (eq? name -); TODO: remove this hack
                       (apply composite-op args)
                       (lambda (out) (apply (cut kind cmd out <...>) args)))))

(define (unary-mutating op out a) (attach (code out a) (op (get (term out)))))
(define-method (unary-functional op out a) (list (op (operand (term out)) (operand (term a)))))
(define-method (unary-functional op out (a <function>)); TODO: cover other cases, refactor
  (intermediate-var (type a) intermediate (code (parameter intermediate) a) (unary-functional op out (parameter intermediate))))

(define (unary-extract op out a) (list (code (term out) (op (term a)))))
(define-syntax-rule (unary-fun name conversion kind op)
  (define-method (name (a <param>)) (make-function name conversion kind op a)))
(define-syntax-rule (unary-asm name conversion kind op)
  (begin (mutating-op op)
         (unary-fun name conversion kind op)))

(define-method (binary-mutating op out a b)
  (if (eqv? (size-of (type b)) (size-of (type out)))
    (attach (code out a) (op (operand (term out)) (operand (term b))))
    (intermediate-var (type out) intermediate (code (parameter intermediate) b) (binary-mutating op out a (parameter intermediate)))))
(define-method (binary-mutating op out a (b <function>)); TODO: cover other cases, refactor
  (intermediate-var (type out) intermediate (code (parameter intermediate) b) (binary-mutating op out a (parameter intermediate))))
(define (binary-functional op out a b)
  (cond ((< (size-of (type b)) (size-of (type a)))
         (intermediate-var (type a) intermediate
                           (code intermediate (term b))
                           (binary-functional op out a (parameter intermediate))))
        ((> (size-of (type b)) (size-of (type a)))
         (intermediate-var (type b) intermediate
                           (code intermediate (term a))
                           (binary-functional op out (parameter intermediate) b)))
        (else (list (op (operand (term out)) (operand (term a)) (operand (term b)))))))
(define-syntax-rule (binary-fun name conversion kind op)
  (define-method (name (a <param>) (b <param>)) (make-function name conversion kind op a b)))
(define-syntax-rule (binary-asm name conversion kind op)
  (begin (mutating-op op)
         (binary-fun name conversion kind op)))

(define-method (returnable self) #f)
(define-method (returnable (self <meta<bool>>)) <ubyte>)
(define-method (returnable (self <meta<int<>>>)) self)
(define (assemble retval vars expr virtual-variables)
  (virtual-variables (if (returnable (class-of retval)) (list (get retval)) '())
                     (concatenate (map content (if (returnable (class-of retval)) vars (cons retval vars))))
                     (attach (code (parameter retval) expr) (RET))))

(define (jit context classes proc); TODO: split up and test
  (let* [(vars        (map skeleton classes))
         (expr        (apply proc (map parameter vars)))
         (result-type (type expr))
         (return-type (returnable result-type))
         (target      (if return-type result-type (pointer result-type)))
         (retval      (skeleton target))
         (args        (if return-type vars (cons retval vars)))
         (code        (asm context
                           (or return-type <null>)
                           (map typecode (concatenate (map content args)))
                           (assemble retval vars expr virtual-variables)))
         (fun         (lambda header (apply code (concatenate (map content header)))))]
    (if return-type
      (lambda args
        (let [(result (apply fun args))]
          (get (build result-type result))))
      (lambda args
        (let [(result (make target #:shape (argmax length (map shape args))))]
          (apply fun (cons result args))
          (get (build result-type result)))))))

(define-syntax-rule (define-unary-dispatch name delegate)
  (define-method (name (a <element>))
    (let [(f (jit ctx (list (class-of a)) delegate))]
      (add-method! name
                   (make <method>
                         #:specializers (list (class-of a))
                         #:procedure f)))
    (name a)))
(define-syntax-rule (define-unary-op define-op conversion kind name op)
  (begin (define-op name conversion kind op)
         (define-unary-dispatch name name)))

(define-method (to-bool a) (to-type <bool> a))
(define-method (to-bool a b) (coerce (to-bool a) (to-bool b)))

(define-method (+ (a <param>)) a)
(define-method (+ (a <element>)) a)
(define-unary-dispatch duplicate identity)
(define-unary-op unary-asm identity unary-mutating   -   NEG)
(define-unary-op unary-asm identity unary-mutating   ~   NOT)
(define-unary-op unary-fun to-bool  unary-functional =0  test-zero)
(define-unary-op unary-fun to-bool  unary-functional !=0 test-non-zero)
(define-unary-op unary-fun to-bool  unary-functional !   test-zero)

(define-syntax-rule (define-binary-delegate name delegate)
 (define-method (name (a <element>) (b <element>))
   (let [(f (jit ctx (map class-of (list a b)) delegate))]
     (add-method! name
                  (make <method>
                        #:specializers (map class-of (list a b))
                        #:procedure (lambda (a b) (f (get a) (get b)))))
     (name a b))))
(define-syntax-rule (define-binary-op define-op conversion kind name op)
  (begin (define-op name conversion kind op)
         (define-method (name (a <element>) b) (name a (make (match b) #:value b)))
         (define-method (name a (b <element>)) (name (make (match a) #:value a) b))
         (define-binary-delegate name name)))
(define-binary-op binary-asm coerce  binary-mutating   +  ADD)
(define-binary-op binary-asm coerce  binary-mutating   -  SUB)
(define-binary-op binary-asm coerce  binary-mutating   *  IMUL)
(define-binary-op binary-fun coerce  binary-functional /  div)
(define-binary-op binary-fun coerce  binary-functional %  mod)
(define-binary-op binary-fun coerce  binary-mutating   << shl)
(define-binary-op binary-fun coerce  binary-mutating   >> shr)
(define-binary-op binary-asm coerce  binary-mutating   &  AND)
(define-binary-op binary-asm coerce  binary-mutating   |  OR)
(define-binary-op binary-asm coerce  binary-mutating   ^  XOR)
(define-binary-op binary-fun to-bool binary-mutating   && bool-and)
(define-binary-op binary-fun to-bool binary-mutating   || bool-or)
(define-binary-op binary-fun to-bool binary-functional =  cmp-equal)
(define-binary-op binary-fun to-bool binary-functional != cmp-not-equal)
(define-binary-op binary-fun to-bool binary-functional <  cmp-lower-than)
(define-binary-op binary-fun to-bool binary-functional <= cmp-lower-equal)
(define-binary-op binary-fun to-bool binary-functional >  cmp-greater-than)
(define-binary-op binary-fun to-bool binary-functional >= cmp-greater-equal)

(define-method (to-type (target <meta<element>>) (a <param>))
  (make <function> #:arguments (list a); TODO: refactor
                   #:type (to-type target (type a))
                   #:project (lambda () (to-type target (body a)))
                   #:term (lambda (out) (code out a ))))

(define-method (to-type (target <meta<element>>) (self <element>))
  (let [(f (jit ctx (list (class-of self)) (cut to-type target <>)))]
    (add-method! to-type
                 (make <method>
                       #:specializers (map class-of (list target self))
                       #:procedure (lambda (target self) (f (get self)))))
    (to-type target self)))

(define (ensure-default-strides img)
  (if (equal? (strides img) (default-strides (shape img))) img (duplicate img)))

;(pointer <rgb<>>)
;(pointer <complex<>>)
;(define-method (parameter   (p <pointer<rgb<>>>))
;  (let [(result (var (typecode p)))
;        (size   (size-of (base (typecode p))))]
;    (make (fragment (typecode p))
;          #:args (list p)
;          #:name parameter
;          #:code (list (MOV (red   result) (ptr (base (typecode p)) (get p)          ))
;                       (MOV (green result) (ptr (base (typecode p)) (get p)      size))
;                       (MOV (blue  result) (ptr (base (typecode p)) (get p) (* 2 size))))
;          #:value result)))
;(define-method (parameter (p <pointer<complex<>>>))
;  (let [(result (var (typecode p)))
;        (size   (size-of (base (typecode p))))]
;    (make (fragment (typecode p))
;          #:args (list p)
;          #:name parameter
;          #:code (list (MOV (real-part result) (ptr (base (typecode p)) (get p)     ))
;                       (MOV (imag-part result) (ptr (base (typecode p)) (get p) size)))
;          #:value result)))
;(define-method (parameter (self <sequence<>>))
;  (make (fragment (class-of self)) #:args (list self) #:name parameter #:code '() #:value self))
;  (define-method (to-type (target <meta<element>>) (frag <fragment<element>>))
;    (let [(source (typecode (type frag)))]
;      (if (eq? target source)
;          frag
;          (let [(result (var target))
;                (mov    (if (>= (size-of source) (size-of target))
;                            mov-part
;                            (if (signed? source)
;                                MOVSX
;                                (if (>= (size-of source) 4) MOV MOVZX))))]
;            (make (fragment (to-type target (type frag)))
;                  #:args (list target frag)
;                  #:name to-type
;                  #:code (append (code frag) (list (mov result (value frag))))
;                  #:value result)))))
;(define (strip-code frag) (parameter (make (type frag) #:value (value frag))))
;(fragment <rgb<>>)
;(fragment <complex<>>)
;(define-method (to-type (target <meta<rgb<>>>) (frag <fragment<element>>))
;  (let* [(tmp    (strip-code frag))
;         (r      (to-type (base target) (red   tmp)))
;         (g      (to-type (base target) (green tmp)))
;         (b      (to-type (base target) (blue  tmp)))
;         (result (rgb r g b))]
;    (make (fragment (to-type target (type frag)))
;          #:args (list target frag)
;          #:name to-type
;          #:code (append (code frag) (code result))
;          #:value (value result))))
;(define-method (to-type (target <meta<complex<>>>) (frag <fragment<element>>))
;  (let* [(tmp    (strip-code frag))
;         (re     (to-type (base target) (real-part tmp)))
;         (im     (to-type (base target) (imag-part tmp)))
;         (result (complex re im))]
;    (make (fragment (to-type target (type frag)))
;          #:args (list target frag)
;          #:name to-type
;          #:code (append (code frag) (code result))
;          #:value (value result))))
;(define-method (rgb (r <fragment<element>>) (g <fragment<element>>) (b <fragment<element>>))
;  (let* [(target (reduce coerce #f (map type (list r g b))))
;         (r~     (to-type (typecode target) r))
;         (g~     (to-type (typecode target) g))
;         (b~     (to-type (typecode target) b))]
;     (make (fragment (rgb target))
;           #:args (list r g b)
;           #:name rgb
;           #:code (append (code r~) (code g~) (code b~))
;           #:value (make <rgb> #:red (value r~) #:green (value g~) #:blue (value b~)))))
;(define-method (complex (real <fragment<element>>) (imag <fragment<element>>))
;  (let* [(target (reduce coerce #f (map type (list real imag))))
;         (real~  (to-type (typecode target) real))
;         (imag~  (to-type (typecode target) imag))]
;     (make (fragment (complex target))
;           #:args (list real imag)
;           #:name complex
;           #:code (append (code real~) (code imag~))
;           #:value (make <internalcomplex> #:real-part (value real~) #:imag-part (value imag~)))))
;(define (mutable-unary op result a)
;  (append (code a) (list (MOV result (value a)) (op result))))
;(define (immutable-unary op result a)
;  (append (code a) (list (op result (value a)))))
;(define-syntax-rule (unary-op name mode op conversion)
;  (define-method (name (a <fragment<element>>))
;    (let* [(target (conversion (type a)))
;           (result (var (typecode target)))]
;      (make (fragment target)
;            #:args (list a)
;            #:name name
;            #:code (mode op result a)
;            #:value result))))
;(define (mutable-binary op result intermediate a b)
;  (let [(a~  (to-type intermediate a))
;        (b~  (to-type intermediate b))
;        (tmp (skeleton intermediate))]
;    (append (code a~) (code b~)
;            (list (MOV result    (value a~))
;                  (MOV (get tmp) (value b~))
;                  (op result (get tmp))))))
;(define (immutable-binary op result intermediate a b)
;  (let [(a~ (to-type intermediate a))
;        (b~ (to-type intermediate b))]
;    (append (code a~) (code b~)
;            (list (op result (value a~) (value b~))))))
;(define (shift-binary op result intermediate a b)
;  (append (code a) (code b) (list (MOV result (value a)) (op result (value b)))))
;(define-method (protect self fun) fun)
;(define-method (protect (self <meta<sequence<>>>) fun) list)
;(define-syntax-rule (binary-op name mode coercion op conversion)
;  (define-method (name (a <fragment<element>>) (b <fragment<element>>))
;    (let* [(intermediate (coercion (type a) (type b)))
;           (target       (conversion intermediate))
;           (result       (var (typecode target)))]
;      (make (fragment target)
;            #:args (list a b)
;            #:name name
;            #:code ((protect intermediate mode) op result intermediate a b)
;            #:value result))))
;
;(define-method (+ (self <fragment<element>>)) self)
;(define-method (conj (self <fragment<int<>>>)) self)
;(define-method (conj (self <fragment<sequence<>>>))
;  (make (class-of self)
;        #:args (list self)
;        #:name conj
;        #:code #f
;        #:value #f))
;
;(binary-op min   immutable-binary sign-space (binary-cmov cmovnle16 cmovnbe16) identity)
;(binary-op max   immutable-binary sign-space (binary-cmov cmovl16 cmovb16)     identity)
;
;(define-method (peel (self <fragment<element>>)) self)
;(define-method (peel (self <fragment<rgb<>>>))
;  (make <rgb> #:red (red self) #:green (green self) #:blue (blue self)))
;(define-method (peel (self <fragment<complex<>>>))
;  (make <internalcomplex> #:real-part (real-part self) #:imag-part (imag-part self)))
;
;(define (do-unary-struct-op op self)
;  (let [(result (op (peel (strip-code self))))]
;    (make (fragment (type self))
;          #:args (list self)
;          #:name op
;          #:code (append (code self) (code result))
;          #:value (value result))))
;(define-syntax-rule (unary-struct-op struct op)
;  (define-method (op (a struct)) (do-unary-struct-op op a)))
;(define (do-binary-struct-op op a b coercion)
;  (let* [(target (coercion (type a) (type b)))
;         (result ((protect target op) (peel (strip-code a)) (peel (strip-code b))))]
;    (make (fragment target)
;          #:args (list a b)
;          #:name op
;          #:code (append (code a) (code b) ((protect target code) result))
;          #:value ((protect target value) result))))
;(define-syntax-rule (binary-struct-op struct op coercion)
;  (begin
;    (define-method (op (a struct) (b struct))
;      (do-binary-struct-op op a b coercion))
;    (define-method (op (a struct) (b <fragment<element>>))
;      (do-binary-struct-op op a b coercion))
;    (define-method (op (a <fragment<element>>) (b struct))
;      (do-binary-struct-op op a b coercion))))
;
;(unary-struct-op  <fragment<rgb<>>> -)
;(unary-struct-op  <fragment<rgb<>>> ~)
;(binary-struct-op <fragment<rgb<>>> +   coerce)
;(binary-struct-op <fragment<rgb<>>> -   coerce)
;(binary-struct-op <fragment<rgb<>>> *   coerce)
;(binary-struct-op <fragment<rgb<>>> &   coerce)
;(binary-struct-op <fragment<rgb<>>> |   coerce)
;(binary-struct-op <fragment<rgb<>>> ^   coerce)
;(binary-struct-op <fragment<rgb<>>> <<  coerce)
;(binary-struct-op <fragment<rgb<>>> >>  coerce)
;(binary-struct-op <fragment<rgb<>>> /   coerce)
;(binary-struct-op <fragment<rgb<>>> %   coerce)
;(binary-struct-op <fragment<rgb<>>> =   (const <bool>))
;(binary-struct-op <fragment<rgb<>>> !=  (const <bool>))
;(binary-struct-op <fragment<rgb<>>> max coerce)
;(binary-struct-op <fragment<rgb<>>> min coerce)
;
;(unary-struct-op  <fragment<complex<>>> -)
;(unary-struct-op  <fragment<complex<>>> conj)
;(binary-struct-op <fragment<complex<>>> + coerce)
;(binary-struct-op <fragment<complex<>>> - coerce)
;(binary-struct-op <fragment<complex<>>> * coerce)
;(binary-struct-op <fragment<complex<>>> / coerce)
;
;(define-method (project self) self)
;(define-method (project (self <fragment<sequence<>>>))
;  (apply (get-name self) (map project (get-args self))))
;(define-method (store (a <element>) (b <fragment<element>>))
;  (append (code b) (list (MOV (get a) (value b)))))
;(define (component self name)
;  (make (fragment (base (type self)))
;          #:args (list self)
;          #:name name
;          #:code (code self)
;          #:value ((protect (type self) name) (value self))))
;(define-method (red   (self <fragment<element>>)) (component self red  ))
;(define-method (green (self <fragment<element>>)) (component self green))
;(define-method (blue  (self <fragment<element>>)) (component self blue ))
;(define-method (real-part (self <fragment<element>>)) self)
;(define-method (real-part (self <fragment<complex<>>>)) (component self real-part))
;(define-method (real-part (self <fragment<sequence<>>>)) (component self real-part))
;(define-method (imag-part (self <fragment<element>>)) (component self imag-part))
;
;(define-method (store (p <pointer<>>) (a <fragment<element>>))
;  (append (code a) (list (MOV (ptr (typecode p) (get p)) (value a)))))
;(define-method (store (p <pointer<>>) (a <fragment<rgb<>>>))
;  (let [(size (size-of (base (typecode p))))]
;    (append (code a)
;            (list (MOV (ptr (base (typecode p)) (get p)           ) (red   (value a)))
;                  (MOV (ptr (base (typecode p)) (get p)       size) (green (value a)))
;                  (MOV (ptr (base (typecode p)) (get p) (* 2 size)) (blue  (value a)))))))
;(define-method (store (p <pointer<>>) (a <fragment<complex<>>>))
;  (let [(size (size-of (base (typecode p))))]
;    (append (code a)
;            (list (MOV (ptr (base (typecode p)) (get p)     ) (real-part (value a)))
;                  (MOV (ptr (base (typecode p)) (get p) size) (imag-part (value a)))))))
;(define-class <elementwise> ()
;  (setup     #:init-keyword #:setup     #:getter get-setup)
;  (increment #:init-keyword #:increment #:getter get-increment)
;  (body      #:init-keyword #:body      #:getter get-body))
;(define-method (element-wise self)
;  (make <elementwise> #:setup '() #:increment '() #:body self))
;(define-method (element-wise (s <sequence<>>))
;  (let [(incr (var <long>))
;        (p    (var <long>))]
;    (make <elementwise>
;          #:setup (list (IMUL incr (last (strides s)) (size-of (typecode s)))
;                        (MOV p (value s)))
;          #:increment (list (ADD p incr))
;          #:body (project (rebase p s)))))
;(define-method (element-wise (self <fragment<sequence<>>>))
;  (let [(loops (map element-wise (get-args self)))]
;    (make <elementwise>
;          #:setup (map get-setup loops)
;          #:increment (map get-increment loops)
;          #:body (apply (get-name self) (map get-body loops)))))
;(define-method (store (s <sequence<>>) (a <fragment<sequence<>>>))
;  (let [(destination (element-wise s))
;        (source      (element-wise a))]
;    (list (get-setup destination)
;          (get-setup source)
;          (repeat (last (shape s))
;                  (append (store (get-body destination) (get-body source))
;                          (get-increment destination)
;                          (get-increment source))))))
;
