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
  #:use-module (aiscm composite)
  #:use-module (aiscm scalar)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm obj)
  #:use-module (aiscm method)
  #:use-module (aiscm sequence)
  #:use-module (aiscm composite)
  #:export (<block> <cmd> <var> <ptr> <param> <indexer> <lookup> <function>
            substitute-variables variables get-args input output labels next-indices
            initial-register-use find-available mark-used-till longest-use live-analysis
            unallocated-variables register-allocations assign-spill-locations add-spill-information
            first-argument replace-variables adjust-stack-pointer
            number-spilled-variables temporary-variables unit-intervals temporary-registers
            sort-live-intervals linear-scan-coloring linear-scan-allocate
            callee-saved save-registers load-registers blocked repeat mov-signed mov-unsigned
            stack-pointer fix-stack-position position-stack-frame
            spill-variable save-and-use-registers register-allocate spill-blocked-predefines
            virtual-variables flatten-code relabel idle-live fetch-parameters spill-parameters
            filter-blocks blocked-intervals native-equivalent var skeleton parameter delegate
            term indexer lookup index type subst code convert-type assemble build-list package-return-content
            jit iterator step setup increment body arguments operand insert-intermediate
            is-pointer? need-conversion? code-needs-intermediate? call-needs-intermediate?
            force-parameters shl shr sign-extend-ax div mod
            test-zero ensure-default-strides unary-extract mutating-code functional-code decompose-value
            decompose-arg delegate-fun generate-return-code
            make-function make-native-function native-call make-constant-function native-const
            scm-eol scm-cons scm-gc-malloc-pointerless scm-gc-malloc)
  #:re-export (min max to-type + - && || ! != ~ & | ^ << >> % =0 !=0 conj)
  #:export-syntax (define-jit-method define-operator-mapping pass-parameters tensor))

(define ctx (make <context>))

; class for defining input and output variables of machine instructions
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
(define-method (equal? (a <cmd>) (b <cmd>)) (equal? (object-slots a) (object-slots b)))

(define-syntax-rule (mutating-op op)
  (define-method (op . args) (make <cmd> #:op op #:io (list (car args)) #:in (cdr args))))
(define-syntax-rule (functional-op op)
  (define-method (op . args) (make <cmd> #:op op #:out (list (car args)) #:in (cdr args))))
(define-syntax-rule (state-setting-op op)
  (define-method (op . args) (make <cmd> #:op op #:in args)))
(define-syntax-rule (state-reading-op op)
  (define-method (op . args) (make <cmd> #:op op #:out args)))

(define (mov-part a b) (MOV a (to-type (integer (* 8 (size-of a)) signed) b)))
(define (mov-cmd movxx movxx32 a b)
  (cond
        ((eqv? (size-of a) (size-of b)) MOV)
        ((<    (size-of a) (size-of b)) mov-part)
        ((eqv? (size-of b) 4)           movxx32)
        (else                           movxx)))
(define-method (mov-signed   (a <operand>) (b <operand>)) ((mov-cmd MOVSX MOVSX a b) a b))
(define-method (mov-unsigned (a <operand>) (b <operand>)) ((mov-cmd MOVZX MOV   a b) a b))
(define (mov a b)
  (list ((if (or (eq? (typecode b) <bool>) (signed? b)) mov-signed mov-unsigned) a b)))

(functional-op    mov-signed  )
(functional-op    mov-unsigned)
(functional-op    MOV         )
(functional-op    MOVSX       )
(functional-op    MOVZX       )
(functional-op    LEA         )
(mutating-op      SHL         )
(mutating-op      SHR         )
(mutating-op      SAL         )
(mutating-op      SAR         )
(state-setting-op PUSH        )
(state-reading-op POP         )
(mutating-op      NEG         )
(mutating-op      NOT         )
(mutating-op      AND         )
(mutating-op      OR          )
(mutating-op      XOR         )
(mutating-op      INC         )
(mutating-op      ADD         )
(mutating-op      SUB         )
(mutating-op      IMUL        )
(mutating-op      IDIV        )
(mutating-op      DIV         )
(state-setting-op CMP         )
(state-setting-op TEST        )
(state-reading-op SETB        )
(state-reading-op SETNB       )
(state-reading-op SETE        )
(state-reading-op SETNE       )
(state-reading-op SETBE       )
(state-reading-op SETNBE      )
(state-reading-op SETL        )
(state-reading-op SETNL       )
(state-reading-op SETLE       )
(state-reading-op SETNLE      )
(mutating-op      CMOVB       )
(mutating-op      CMOVNB      )
(mutating-op      CMOVE       )
(mutating-op      CMOVNE      )
(mutating-op      CMOVBE      )
(mutating-op      CMOVNBE     )
(mutating-op      CMOVL       )
(mutating-op      CMOVNL      )
(mutating-op      CMOVLE      )
(mutating-op      CMOVNLE     )

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
(define-method (equal? (a <ptr>) (b <ptr>)) (equal? (object-slots a) (object-slots b)))
(define-method (ptr (type <meta<element>>) . args) (make <ptr> #:type type #:args args))
(define-method (variables self) '())
(define-method (variables (self <var>)) (list self))
(define-method (variables (self <cmd>)) (variables (get-args self)))
(define-method (variables (self <ptr>)) (variables (get-args self)))
(define-method (variables (self <list>)) (delete-duplicates (append-map variables self)))
(define-method (input (self <cmd>))
  (delete-duplicates (variables (append (get-input self) (filter (cut is-a? <> <ptr>) (get-args self))))))
(define-method (output (self <cmd>)) (variables (get-output self)))
(define-method (substitute-variables self alist) self)

(define-method (substitute-variables (self <var>) alist)
  (let [(target (assq-ref alist self))]
    (if (is-a? target <register>)
      (to-type (typecode self) target)
      (or target self))))
(define-method (substitute-variables (self <ptr>) alist)
  (let [(target (substitute-variables (car (get-args self)) alist))]
    (if (is-a? target <pair>)
      (ptr (typecode self) (car target) (+ (cadr (get-args self)) (cdr target)))
      (apply ptr (typecode self) target (cdr (get-args self))))))
(define-method (substitute-variables (self <cmd>) alist)
  (apply (get-op self) (map (cut substitute-variables <> alist) (get-args self))))
(define-method (substitute-variables (self <list>) alist) (map (cut substitute-variables <> alist) self))

(define-method (native-type (i <real>) . args); TODO: remove this when floating point support is ready
  (if (every real? args)
      <obj>
      (apply native-type (sort-by-pred (cons i args) real?))))

(define-method (native-equivalent  self                   ) #f      )
(define-method (native-equivalent (self <meta<bool>>     )) <ubyte> )
(define-method (native-equivalent (self <meta<int<>>>    )) self    )
(define-method (native-equivalent (self <meta<float<>>>  )) self    )
(define-method (native-equivalent (self <meta<obj>>      )) <ulong> )
(define-method (native-equivalent (self <meta<pointer<>>>)) <ulong> )

(define-method (var self) (make <var> #:type (native-equivalent self)))

(define (labels prog)
  "Get positions of labels in program"
  (filter (compose symbol? car) (map cons prog (iota (length prog)))))

(define (initial-register-use registers)
  "Initially all registers are available from index zero on"
  (map (cut cons <> 0) registers))

(define (sort-live-intervals live-intervals predefined-variables)
  "Sort live intervals predefined variables first and then lexically by start point and length of interval"
  (sort-by live-intervals
           (lambda (live) (if (memv (car live) predefined-variables) -1 (- (cadr live) (/ 1 (+ 2 (cddr live))))))))

(define (find-available availability first-index)
  "Find element available from the specified first program index onwards"
  (car (or (find (compose (cut <= <> first-index) cdr) availability) '(#f))))

(define (mark-used-till availability element last-index)
  "Mark element in use up to specified index"
  (assq-set availability element (1+ last-index)))

(define (longest-use availability)
  "Select register blocking for the longest time as a spill candidate"
  (car (argmax cdr availability)))

(define-method (next-indices labels cmd k)
  "Determine next program indices for a statement"
  (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices labels (cmd <jcc>) k)
  "Determine next program indices for a (conditional) jump"
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (conditional? cmd) (list (1+ k) target) (list target))))

(define (live-analysis prog results)
  "Get list of live variables for program terminated by RET statement"
  (letrec* [(inputs    (map-if (cut equal? (RET) <>) (const results) input prog))
            (outputs   (map output prog))
            (indices   (iota (length prog)))
            (lut       (labels prog))
            (flow      (map (cut next-indices lut <...>) prog indices))
            (same?     (cut every (cut lset= equal? <...>) <...>))
            (track     (lambda (value)
                         (lambda (in ind out)
                           (union in (difference (apply union (map (cut list-ref value <>) ind)) out)))))
            (initial   (map (const '()) prog))
            (iteration (lambda (value) (map (track value) inputs flow outputs)))]
    (map union (fixed-point initial iteration same?) outputs)))

(define (unallocated-variables allocation)
   "Return a list of unallocated variables"
   (map car (filter (compose not cdr) allocation)))

(define (register-allocations allocation)
   "Return a list of variables with register allocated"
   (filter cdr allocation))

(define (assign-spill-locations variables offset increment)
  "Assign spill locations to a list of variables"
  (map (lambda (variable index) (cons variable (ptr (typecode variable) RSP index)))
       variables
       (iota (length variables) offset increment)))

(define (add-spill-information allocation offset increment)
  "Allocate spill locations for spilled variables"
  (append (register-allocations allocation)
          (assign-spill-locations (unallocated-variables allocation) offset increment)))

(define (linear-scan-coloring live-intervals registers predefined)
  "Linear scan register allocation based on live intervals"
  (define (linear-allocate live-intervals register-use variable-use allocation)
    (if (null? live-intervals)
        allocation
        (let* [(candidate    (car live-intervals))
               (variable     (car candidate))
               (interval     (cdr candidate))
               (first-index  (car interval))
               (last-index   (cdr interval))
               (variable-use (mark-used-till variable-use variable last-index))
               (register     (or (assq-ref predefined variable)
                                 (find-available register-use first-index)))
               (recursion    (lambda (allocation register)
                               (linear-allocate (cdr live-intervals)
                                                (mark-used-till register-use register last-index)
                                                variable-use
                                                (assq-set allocation variable register))))]
          (if register
            (recursion allocation register)
            (let* [(spill-candidate (longest-use variable-use))
                   (register        (assq-ref allocation spill-candidate))]
              (recursion (assq-set allocation spill-candidate #f) register))))))
  (linear-allocate (sort-live-intervals live-intervals (map car predefined))
                   (initial-register-use registers)
                   '()
                   '()))

(define-method (first-argument self)
   "Return false for compiled instructions"
   #f)
(define-method (first-argument (self <cmd>))
   "Get first argument of machine instruction"
   (car (get-args self)))

(define (replace-variables cmd allocation temporary)
  "Replace variables with registers and add spill code if necessary"
  (let* [(primary-argument (first-argument cmd))
         (primary-location (assq-ref allocation primary-argument))]
    (if (is-a? primary-location <address>)
      (let [(register (to-type (typecode primary-argument) temporary))]
        (compact (and (memv primary-argument (input cmd)) (MOV register primary-location))
                 (substitute-variables cmd (assq-set allocation primary-argument temporary))
                 (and (memv primary-argument (output cmd)) (MOV primary-location register))))
      (list (substitute-variables cmd allocation)))))

(define (adjust-stack-pointer offset prog)
  "Adjust stack pointer offset at beginning and end of program"
  (append (list (SUB RSP offset)) (all-but-last prog) (list (ADD RSP offset) (RET))))

(define (number-spilled-variables allocation)
  "Count the number of spilled variables"
  (length (unallocated-variables allocation)))

(define (temporary-variables prog)
  "Allocate temporary variable for each first argument of an instruction"
  (map (lambda (cmd) (let [(arg (first-argument cmd))] (and arg (var (typecode arg))))) prog))

(define (unit-intervals vars)
  "Generate intervals of length one for each temporary variable"
  (filter car (map (lambda (var index) (cons var (cons index index))) vars (iota (length vars)))))

(define (temporary-registers allocation variables)
  "Look up register for each temporary variable given the result of a register allocation"
  (map (lambda (var) (let [(reg (assq-ref allocation var))] (and reg (to-type (typecode var) reg)))) variables))

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (predefined '()))
  "Linear scan register allocation for a given program"
  (let* [(live                (live-analysis prog '())); TODO: specify return values here
         (temporary-variables (temporary-variables prog))
         (intervals           (append (live-intervals live (variables prog))
                                      (unit-intervals temporary-variables)))
         (allocation          (linear-scan-coloring intervals registers predefined))
         (temporaries         (temporary-registers allocation temporary-variables))
         (stack-offset        (* 8 (1+ (number-spilled-variables allocation))))
         (locations           (add-spill-information allocation 8 8))]
    (adjust-stack-pointer stack-offset
                          (concatenate (map (lambda (cmd register) (replace-variables cmd locations register)) prog temporaries)))))

; stack pointer placeholder
(define stack-pointer (make <var> #:type <long> #:symbol 'stack-pointer))
; methods to replace stack pointer placeholder with RSP + offset
(define-method (fix-stack-position self offset) self)
(define-method (fix-stack-position (self <cmd>) offset)
  (apply (get-op self) (map (cut fix-stack-position <> offset) (get-args self))))
(define-method (fix-stack-position (self <ptr>) offset)
  (if (equal? stack-pointer (car (get-args self)))
    (apply ptr (list (typecode self) RSP (- (cadr (get-args self)) offset)))
    self))
(define-method (fix-stack-position (self <list>) offset) (map (cut fix-stack-position <> offset) self))

(define (position-stack-frame prog offset)
  (append (list (SUB RSP (- offset)))
          (all-but-last (fix-stack-position prog offset))
          (list (ADD RSP (- offset)) (RET))))

; RSP is not included because it is used as a stack pointer
; RBP is not included because it may be used as a frame pointer
(define default-registers (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX R12 R13 R14 R15))
(define caller-saved (list RAX RCX RDX RSI RDI R10 R11 R9 R8))
(define register-parameters (list RDI RSI RDX RCX R8 R9))
(define (callee-saved registers)
  (lset-intersection eq? (delete-duplicates registers) (list RBX RBP RSP R12 R13 R14 R15)))
(define (save-registers registers offset)
  (map (lambda (register offset) (MOV (ptr <long> stack-pointer offset) register))
       registers (iota (length registers) offset -8)))
(define (load-registers registers offset)
  (map (lambda (register offset) (MOV register (ptr <long> stack-pointer offset)))
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

; replace target with a temporary variable and add instructions for fetching and storing it as required
(define ((insert-temporary target) cmd)
  (let [(temporary (var (typecode target)))]
    (compact
      (and (memv target (input cmd)) (MOV temporary target))
      (substitute-variables cmd (list (cons target temporary)))
      (and (memv target (output cmd)) (MOV target temporary)))))
(define (spill-variable target location prog)
  (substitute-variables (map (insert-temporary target) prog) (list (cons target location))))

(define ((idle-live prog live) var)
  (count (lambda (cmd active) (and (not (memv var (variables cmd))) (memv var active))) prog live))
(define ((spill-parameters parameters) colors)
  (filter-map (lambda (parameter register)
    (let [(value (assq-ref colors parameter))]
      (if (not (is-a? value <register>)) (MOV value (to-type (typecode parameter) register)) #f)))
    parameters
    register-parameters))
(define ((fetch-parameters parameters) colors)
  (filter-map (lambda (parameter offset)
    (let [(value (assq-ref colors parameter))]
      (if (is-a? value <register>) (MOV (to-type (typecode parameter) value) (ptr (typecode parameter) stack-pointer offset)) #f)))
    parameters
    (iota (length parameters) 8 8)))
(define (save-and-use-registers prog colors parameters offset)
  (let [(need-saving          (callee-saved (map cdr colors)))
        (first-six-parameters (take-up-to parameters 6))
        (remaining-parameters (drop-up-to parameters 6))]
    (append (save-registers need-saving offset)
            ((spill-parameters first-six-parameters) colors)
            ((fetch-parameters remaining-parameters) colors)
            (all-but-last (substitute-variables prog colors))
            (load-registers need-saving offset)
            (list (RET)))))

(define (with-spilled-variable target location prog predefined blocked fun)
  (let* [(spill-code (spill-variable target location prog))]
    (fun (flatten-code spill-code)
         (assq-set predefined target location)
         (update-intervals blocked (index-groups spill-code)))))

(define (with-register-allocation prog predefined blocked registers parameters offset fun)
  (let* [(live       (live-analysis prog '()))
         (all-vars   (delete stack-pointer (variables prog)))
         (vars       (difference all-vars (map car predefined)))
         (intervals  (live-intervals live all-vars))
         (adjacent   (overlap intervals))
         (colors     (color-intervals intervals vars registers #:predefined predefined #:blocked blocked))
         (unassigned (find (compose not cdr) (reverse colors)))]
    (if unassigned
      (let* [(target       (argmax (idle-live prog live) (adjacent (car unassigned))))
             (stack-param? (and (index-of target parameters) (>= (index-of target parameters) 6)))
             (new-offset   (if stack-param? offset (- offset 8)))
             (displacement (if stack-param? (* 8 (- (index-of target parameters) 5)) offset))
             (location     (ptr (typecode target) stack-pointer displacement))]
        (with-spilled-variable target location prog predefined blocked
          (lambda (prog predefined blocked)
            (with-register-allocation prog predefined blocked registers parameters new-offset fun))))
      (fun prog colors parameters offset))))

(define* (register-allocate prog #:key (predefined '())
                                       (blocked '())
                                       (registers default-registers)
                                       (parameters '())
                                       (offset -8))
  (with-register-allocation prog predefined blocked registers parameters offset
    (lambda (prog colors parameters offset)
      (position-stack-frame (save-and-use-registers prog colors parameters offset) offset))))

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
               (location (ptr (typecode target) stack-pointer offset))]
        (with-spilled-variable target location prog predefined blocked
          (lambda (prog predefined blocked)
            (spill-blocked-predefines prog
                                      #:predefined predefined
                                      #:blocked    blocked
                                      #:registers  registers
                                      #:parameters parameters
                                      #:offset     (- offset 8)))))
      (apply register-allocate (cons prog args))))))

(define* (virtual-variables result-vars arg-vars instructions #:key (registers default-registers))
  (let* [(result-regs  (map cons result-vars (list RAX)))
         (arg-regs     (map cons arg-vars register-parameters))]
    (spill-blocked-predefines (flatten-code (relabel (filter-blocks instructions)))
                              #:predefined (append result-regs arg-regs)
                              #:blocked    (blocked-intervals instructions)
                              #:registers  registers
                              #:parameters arg-vars)))

(define (repeat n . body)
  (let [(i (var (typecode n)))]
    (list (MOV i 0) 'begin (CMP i n) (JE 'end) (INC i) body (JMP 'begin) 'end)))

(define-class <block> ()
  (reg  #:init-keyword #:reg  #:getter get-reg)
  (code #:init-keyword #:code #:getter get-code))
(define-method (blocked (reg <register>) . body) (make <block> #:reg reg #:code body))
(define-method (blocked (lst <null>) . body) body)
(define-method (blocked (lst <pair>) . body) (blocked (car lst) (apply blocked (cdr lst) body)))
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
(define (div/mod-prepare-signed r a)
  (list (MOV (to-type (typecode r) RAX) a) (sign-extend-ax (size-of r))))
(define (div/mod-prepare-unsigned r a)
  (if (eqv? 1 (size-of r)) (list (MOVZX AX a)) (list (MOV (to-type (typecode r) RAX) a) (MOV (to-type (typecode r) RDX) 0))))
(define (div/mod-signed r a b) (attach (div/mod-prepare-signed r a) (IDIV b)))
(define (div/mod-unsigned r a b) (attach (div/mod-prepare-unsigned r a) (DIV b)))
(define (div/mod-block-registers r . code) (blocked RAX (if (eqv? 1 (size-of r)) code (blocked RDX code))))
(define (div/mod r a b . finalise) (div/mod-block-registers r ((if (signed? r) div/mod-signed div/mod-unsigned) r a b) finalise))
(define (div r a b) (div/mod r a b (MOV r (to-type (typecode r) RAX))))
(define (mod r a b) (div/mod r a b (if (eqv? 1 (size-of r)) (list (MOV AL AH) (MOV r AL)) (MOV r DX))))

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
(define ((cmp-setxx set-signed set-unsigned) out a b)
  (let [(set (if (or (signed? a) (signed? b)) set-signed set-unsigned))]
    (attach (cmp a b) (set out))))
(define cmp-equal         (cmp-setxx SETE   SETE  ))
(define cmp-not-equal     (cmp-setxx SETNE  SETNE ))
(define cmp-lower-than    (cmp-setxx SETL   SETB  ))
(define cmp-lower-equal   (cmp-setxx SETLE  SETBE ))
(define cmp-greater-than  (cmp-setxx SETNLE SETNBE))
(define cmp-greater-equal (cmp-setxx SETNL  SETNB ))

(define ((cmp-cmovxx set-signed set-unsigned jmp-signed jmp-unsigned) r a b)
  (if (eqv? 1 (size-of r))
    (append (mov r a) (cmp r b) (list ((if (signed? r) jmp-signed jmp-unsigned) 'skip)) (mov r b) (list 'skip))
    (append (mov r a) (cmp r b) (list ((if (signed? r) set-signed set-unsigned) r b)))))
(define minor (cmp-cmovxx CMOVNLE CMOVNBE JL   JB  ))
(define major (cmp-cmovxx CMOVL   CMOVB   JNLE JNBE))

(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<sequence<>>>))
  (let [(slice (skeleton (project self)))]
    (make self
          #:value   (value slice)
          #:shape   (cons (var <long>) (shape   slice))
          #:strides (cons (var <long>) (strides slice)))))

(define-class <param> ()
  (delegate #:init-keyword #:delegate #:getter delegate))

(define-class <indexer> (<param>)
  (dimension #:init-keyword #:dimension #:getter dimension)
  (index     #:init-keyword #:index     #:getter index))
(define (indexer dimension index delegate)
  (make <indexer> #:dimension dimension #:index index #:delegate delegate))

(define-class <lookup> (<param>)
  (index    #:init-keyword #:index    #:getter index)
  (stride   #:init-keyword #:stride   #:getter stride)
  (iterator #:init-keyword #:iterator #:getter iterator)
  (step     #:init-keyword #:step     #:getter step))
(define-method (lookup index delegate stride iterator step)
  (make <lookup> #:index index #:delegate delegate #:stride stride #:iterator iterator #:step step))
(define-method (lookup idx (obj <indexer>) stride iterator step)
  (indexer (dimension obj) (index obj) (lookup idx (delegate obj) stride iterator step)))

(define-class <function> (<param>)
  (arguments #:init-keyword #:arguments #:getter arguments)
  (type      #:init-keyword #:type      #:getter type)
  (project   #:init-keyword #:project   #:getter project)
  (term      #:init-keyword #:term      #:getter term))

(define-method (type (self <param>)) (typecode (delegate self)))
(define-method (type (self <indexer>)) (sequence (type (delegate self))))
(define-method (type (self <lookup>)) (type (delegate self)))
(define-method (typecode (self <indexer>)) (typecode (type self)))
(define-method (shape (self <indexer>)) (attach (shape (delegate self)) (dimension self)))
(define-method (strides (self <indexer>)) (attach (strides (delegate self)) (stride (lookup self (index self)))))
(define-method (lookup (self <indexer>)) (lookup self (index self)))
(define-method (lookup (self <indexer>) (idx <var>)) (lookup (delegate self) idx))
(define-method (lookup (self <lookup>) (idx <var>)) (if (eq? (index self) idx) self (lookup (delegate self) idx)))
(define-method (stride (self <indexer>)) (stride (lookup self)))
(define-method (iterator (self <indexer>)) (iterator (lookup self)))
(define-method (step (self <indexer>)) (step (lookup self)))
(define-method (parameter (self <element>)) (make <param> #:delegate self))
(define-method (parameter (self <sequence<>>))
  (let [(idx (var <long>))]
    (indexer (parameter (make <long> #:value (dimension self)))
             idx
             (lookup idx
                     (parameter (project self))
                     (parameter (make <long> #:value (stride self)))
                     (var <long>)
                     (var <long>)))))
(define-method (parameter (self <meta<element>>)) (parameter (skeleton self)))
(define-method (subst self candidate replacement) self)
(define-method (subst (self <indexer>) candidate replacement)
  (indexer (dimension self) (index self) (subst (delegate self) candidate replacement)))
(define-method (subst (self <lookup>) candidate replacement)
  (lookup (if (eq? (index self) candidate) replacement (index self))
          (subst (delegate self) candidate replacement)
          (stride self)
          (iterator self)
          (step self)))
(define-method (value (self <param>)) (value (delegate self)))
(define-method (value (self <indexer>)) (value (delegate self)))
(define-method (value (self <lookup>)) (value (delegate self)))
(define-method (rebase value (self <param>)) (parameter (rebase value (delegate self))))
(define-method (rebase value (self <indexer>))
  (indexer (dimension self) (index self) (rebase value (delegate self))))
(define-method (rebase value (self <lookup>))
  (lookup (index self) (rebase value (delegate self)) (stride self) (iterator self) (step self)))
(define-method (project (self <indexer>)) (project (delegate self) (index self)))
(define-method (project (self <indexer>) (idx <var>))
  (indexer (dimension self) (index self) (project (delegate self) idx)))
(define-method (project (self <lookup>) (idx <var>))
  (if (eq? (index self) idx)
      (delegate self)
      (lookup (index self) (project (delegate self) idx) (stride self) (iterator self) (step self))))
(define-method (get (self <indexer>) idx) (subst (delegate self) (index self) idx))
(define-syntax-rule (tensor size index expr) (let [(index (var <long>))] (indexer size index expr)))

(define-method (size-of (self <param>))
  (apply * (native-const <long> (size-of (typecode (type self)))) (shape self)))

(define-method (setup self) '())
(define-method (setup (self <indexer>))
  (list (IMUL (step self) (get (delegate (stride self))) (size-of (typecode self)))
        (MOV (iterator self) (value self))))
(define-method (setup (self <function>)) (append-map setup (arguments self)))
(define-method (increment self) '())
(define-method (increment (self <indexer>)) (list (ADD (iterator self) (step self))))
(define-method (increment (self <function>)) (append-map increment (arguments self)))
(define-method (body self) self)
(define-method (body (self <indexer>)) (project (rebase (iterator self) self)))
(define-method (body (self <function>)) ((project self)))
(define-method (shape (self <function>)) (argmax length (map shape (arguments self))))

(define-method (operand (a <element>)) (get a))
(define-method (operand (a <pointer<>>))
  (if (pointer-offset a)
      (ptr (typecode a) (get a) (pointer-offset a))
      (ptr (typecode a) (get a))))
(define-method (operand (a <param>)) (operand (delegate a)))

(define (insert-intermediate value intermediate fun)
  (append (code intermediate value) (fun intermediate)))

(define-method (code (a <element>) (b <element>)) ((to-type (typecode a) (typecode b)) (parameter a) (list (parameter b))))
(define-method (code (a <element>) (b <integer>)) (list (MOV (operand a) b)))

(define-method (code (a <pointer<>>) (b <pointer<>>))
  (insert-intermediate b (skeleton (typecode a)) (cut code a <>)))
(define-method (code (a <param>) (b <param>)) (code (delegate a) (delegate b)))
(define-method (code (a <indexer>) (b <param>))
  (list (setup a)
        (setup b)
        (repeat (get (delegate (dimension a)))
                (append (code (body a) (body b))
                        (increment a)
                        (increment b)))))
(define-method (code (out <element>) (fun <function>))
  (if (need-conversion? (typecode out) (type fun))
    (insert-intermediate fun (skeleton (type fun)) (cut code out <>))
    ((term fun) (parameter out))))
(define-method (code (out <pointer<>>) (fun <function>))
  (insert-intermediate fun (skeleton (typecode out)) (cut code out <>)))
(define-method (code (out <param>) (fun <function>)) (code (delegate out) fun))
(define-method (code (out <param>) (value <integer>)) (code out (native-const (type out) value)))

; decompose parameters into elementary native types
(define-method (content (type <meta<element>>) (self <param>)) (map parameter (content type (delegate self))))
(define-method (content (type <meta<scalar>>) (self <function>)) (list self))
(define-method (content (type <meta<composite>>) (self <function>)) (arguments self))
(define-method (content (type <meta<sequence<>>>) (self <param>))
  (cons (dimension self) (cons (stride self) (content (project type) (project self)))))

(define (is-pointer? value) (and (delegate value) (is-a? (delegate value) <pointer<>>)))
(define-method (need-conversion? target type) (not (eq? target type)))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<bool>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<bool>>))
  (not (eqv? (size-of target) (size-of type))))
(define (code-needs-intermediate? t value) (or (is-a? value <function>) (need-conversion? t (type value))))
(define (call-needs-intermediate? t value) (or (is-pointer? value) (code-needs-intermediate? t value)))
(define-method (force-parameters (targets <list>) args predicate fun)
  (let* [(mask          (map predicate targets args))
         (intermediates (map-select mask (compose parameter car list) (compose cadr list) targets args))
         (preamble      (concatenate (map-select mask code (const '()) intermediates args)))]
    (attach preamble (apply fun intermediates))))
(define-method (force-parameters target args predicate fun)
  (force-parameters (make-list (length args) target) args predicate fun))

(define (operation-code target op out args)
  "Adapter for nested expressions"
  (force-parameters target args code-needs-intermediate?
    (lambda intermediates (apply op (operand out) (map operand intermediates)))))
(define ((functional-code op) out args)
  "Adapter for machine code without side effects on its arguments"
  (operation-code (reduce coerce #f (map type args)) op out args))
(define ((mutating-code op) out args)
  "Adapter for machine code overwriting its first argument"
  (insert-intermediate (car args) out (cut operation-code (type out) op <> (cdr args))))
(define ((unary-extract op) out args)
  "Adapter for machine code to extract part of a composite value"
  (code (delegate out) (apply op (map delegate args))))

(define-macro (define-operator-mapping name arity type fun)
  (let* [(args   (symbol-list arity))
         (header (typed-header args type))]
    `(define-method (,name . ,header) ,fun)))

(define-operator-mapping -   1 <meta<int<>>> (mutating-code   NEG              ))
(define-method (- (z <integer>) (a <meta<int<>>>)) (mutating-code NEG))
(define-operator-mapping ~   1 <meta<int<>>> (mutating-code   NOT              ))
(define-operator-mapping =0  1 <meta<int<>>> (functional-code test-zero        ))
(define-operator-mapping !=0 1 <meta<int<>>> (functional-code test-non-zero    ))
(define-operator-mapping !   1 <meta<bool>>  (functional-code test-zero        ))
(define-operator-mapping +   2 <meta<int<>>> (mutating-code   ADD              ))
(define-operator-mapping -   2 <meta<int<>>> (mutating-code   SUB              ))
(define-operator-mapping *   2 <meta<int<>>> (mutating-code   IMUL             ))
(define-operator-mapping /   2 <meta<int<>>> (functional-code div              ))
(define-operator-mapping %   2 <meta<int<>>> (functional-code mod              ))
(define-operator-mapping <<  2 <meta<int<>>> (mutating-code   shl              ))
(define-operator-mapping >>  2 <meta<int<>>> (mutating-code   shr              ))
(define-operator-mapping &   2 <meta<int<>>> (mutating-code   AND              ))
(define-operator-mapping |   2 <meta<int<>>> (mutating-code   OR               ))
(define-operator-mapping ^   2 <meta<int<>>> (mutating-code   XOR              ))
(define-operator-mapping &&  2 <meta<bool>>  (mutating-code   bool-and         ))
(define-operator-mapping ||  2 <meta<bool>>  (mutating-code   bool-or          ))
(define-operator-mapping =   2 <meta<int<>>> (functional-code cmp-equal        ))
(define-operator-mapping !=  2 <meta<int<>>> (functional-code cmp-not-equal    ))
(define-operator-mapping <   2 <meta<int<>>> (functional-code cmp-lower-than   ))
(define-operator-mapping <=  2 <meta<int<>>> (functional-code cmp-lower-equal  ))
(define-operator-mapping >   2 <meta<int<>>> (functional-code cmp-greater-than ))
(define-operator-mapping >=  2 <meta<int<>>> (functional-code cmp-greater-equal))
(define-operator-mapping min 2 <meta<int<>>> (functional-code minor            ))
(define-operator-mapping max 2 <meta<int<>>> (functional-code major            ))

(define-operator-mapping -   1 <meta<element>> (native-fun obj-negate    ))
(define-method (- (z <integer>) (a <meta<element>>)) (native-fun obj-negate))
(define-operator-mapping ~   1 <meta<element>> (native-fun scm-lognot    ))
(define-operator-mapping =0  1 <meta<element>> (native-fun obj-zero-p    ))
(define-operator-mapping !=0 1 <meta<element>> (native-fun obj-nonzero-p ))
(define-operator-mapping !   1 <meta<element>> (native-fun obj-not       ))
(define-operator-mapping +   2 <meta<element>> (native-fun scm-sum       ))
(define-operator-mapping -   2 <meta<element>> (native-fun scm-difference))
(define-operator-mapping *   2 <meta<element>> (native-fun scm-product   ))
(define-operator-mapping /   2 <meta<element>> (native-fun scm-divide    ))
(define-operator-mapping %   2 <meta<element>> (native-fun scm-remainder ))
(define-operator-mapping <<  2 <meta<element>> (native-fun scm-ash       ))
(define-operator-mapping >>  2 <meta<element>> (native-fun obj-shr       ))
(define-operator-mapping &   2 <meta<element>> (native-fun scm-logand    ))
(define-operator-mapping |   2 <meta<element>> (native-fun scm-logior    ))
(define-operator-mapping ^   2 <meta<element>> (native-fun scm-logxor    ))
(define-operator-mapping &&  2 <meta<element>> (native-fun obj-and       ))
(define-operator-mapping ||  2 <meta<element>> (native-fun obj-or        ))
(define-operator-mapping =   2 <meta<element>> (native-fun obj-equal-p   ))
(define-operator-mapping !=  2 <meta<element>> (native-fun obj-nequal-p  ))
(define-operator-mapping <   2 <meta<element>> (native-fun obj-less-p    ))
(define-operator-mapping <=  2 <meta<element>> (native-fun obj-leq-p     ))
(define-operator-mapping >   2 <meta<element>> (native-fun obj-gr-p      ))
(define-operator-mapping >=  2 <meta<element>> (native-fun obj-geq-p     ))
(define-operator-mapping min 2 <meta<element>> (native-fun scm-min       ))
(define-operator-mapping max 2 <meta<element>> (native-fun scm-max       ))

(define-method (decompose-value (target <meta<scalar>>) self) self)

(define-method (delegate-op (target <meta<scalar>>) (intermediate <meta<scalar>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op (target <meta<sequence<>>>) (intermediate <meta<sequence<>>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op target intermediate name out args)
  (let [(result (apply name (map (lambda (arg) (decompose-value (type arg) arg)) args)))]
    (append-map code (content (type out) out) (content (type result) result))))
(define (delegate-fun name)
  (lambda (out args) (delegate-op (type out) (reduce coerce #f (map type args)) name out args)))

(define (make-function name coercion fun args)
  (make <function> #:arguments args
                   #:type      (apply coercion (map type args))
                   #:project   (lambda ()  (apply name (map body args)))
                   #:delegate  #f
                   #:term      (lambda (out) (fun out args))))

(define-macro (n-ary-base name arity coercion fun)
  (let* [(args   (symbol-list arity))
         (header (typed-header args '<param>))]
    `(define-method (,name . ,header) (make-function ,name ,coercion ,fun (list . ,args)))))

(define (content-vars args) (map get (append-map content (map class-of args) args)))

(define (assemble return-args args instructions)
  "Determine result variables, argument variables, and instructions"
  (list (content-vars return-args) (content-vars args) (attach instructions (RET))))

(define (build-list . args)
  "Generate code to package ARGS in a Scheme list"
  (fold-right scm-cons scm-eol args))

(define (package-return-content value)
  "Generate code to package parameter VALUE in a Scheme list"
  (apply build-list (content (type value) value)))

(define-method (construct-value result-type retval expr) '())
(define-method (construct-value (result-type <meta<sequence<>>>) retval expr)
  (let [(malloc (if (pointerless? result-type) scm-gc-malloc-pointerless scm-gc-malloc))]
    (append (append-map code (shape retval) (shape expr))
            (code (last (content result-type retval)) (malloc (size-of retval)))
            (append-map code (strides retval) (default-strides (shape retval))))))

(define (generate-return-code args intermediate expr)
  (let [(retval (skeleton <obj>))]
    (list (list retval)
          args
          (append (construct-value (type intermediate) intermediate expr)
                  (code intermediate expr)
                  (code (parameter retval) (package-return-content intermediate))))))

(define (jit context classes proc)
  (let* [(vars         (map skeleton classes))
         (expr         (apply proc (map parameter vars)))
         (result-type  (type expr))
         (result       (parameter result-type))
         (types        (map class-of vars))
         (intermediate (generate-return-code vars result expr))
         (instructions (asm context
                            <ulong>
                            (map typecode (content-vars vars))
                            (apply virtual-variables (apply assemble intermediate))))
         (fun          (lambda header (apply instructions (append-map unbuild types header))))]
    (lambda args (build result-type (address->scm (apply fun args))))))

(define-macro (define-jit-dispatch name arity delegate)
  (let* [(args   (symbol-list arity))
         (header (typed-header args '<element>))]
    `(define-method (,name . ,header)
       (let [(f (jit ctx (map class-of (list . ,args)) ,delegate))]
         (add-method! ,name
                      (make <method>
                            #:specializers (map class-of (list . ,args))
                            #:procedure (lambda args (apply f (map get args))))))
       (,name . ,args))))

(define-macro (define-nary-collect name arity)
  (let* [(args   (symbol-list arity))
         (header (cons (list (car args) '<element>) (cdr args)))]; TODO: extract and test
    (cons 'begin
          (map
            (lambda (i)
              `(define-method (,name . ,(cycle-times header i))
                (apply ,name (map wrap (list . ,(cycle-times args i))))))
            (iota arity)))))

(define-syntax-rule (define-jit-method coercion name arity)
  (begin (n-ary-base name arity coercion (delegate-fun name))
         (define-nary-collect name arity)
         (define-jit-dispatch name arity name)))

; various type class conversions
(define-method (convert-type (target <meta<element>>) (self <meta<element>>)) target)
(define-method (convert-type (target <meta<element>>) (self <meta<sequence<>>>)) (multiarray target (dimensions self)))
(define-method (to-bool a) (convert-type <bool> a))
(define-method (to-bool a b) (coerce (to-bool a) (to-bool b)))

; define unary and binary operations
(define-method (+ (a <param>)) a)
(define-method (+ (a <element>)) a)
(define-method (* (a <param>)) a)
(define-method (* (a <element>)) a)
(define-jit-dispatch duplicate 1 identity)
(define-jit-method identity -   1)
(define-jit-method identity ~   1)
(define-jit-method to-bool  =0  1)
(define-jit-method to-bool  !=0 1)
(define-jit-method to-bool  !   1)
(define-jit-method coerce   +   2)
(define-jit-method coerce   -   2)
(define-jit-method coerce   *   2)
(define-jit-method coerce   /   2)
(define-jit-method coerce   %   2)
(define-jit-method coerce   <<  2)
(define-jit-method coerce   >>  2)
(define-jit-method coerce   &   2)
(define-jit-method coerce   |   2)
(define-jit-method coerce   ^   2)
(define-jit-method coerce   &&  2)
(define-jit-method coerce   ||  2)
(define-jit-method to-bool  =   2)
(define-jit-method to-bool  !=  2)
(define-jit-method to-bool  <   2)
(define-jit-method to-bool  <=  2)
(define-jit-method to-bool  >   2)
(define-jit-method to-bool  >=  2)
(define-jit-method coerce   min 2)
(define-jit-method coerce   max 2)

(define-method (to-type (target <meta<ubyte>>) (source <meta<obj>>  )) (native-fun scm-to-uint8   ))
(define-method (to-type (target <meta<byte>> ) (source <meta<obj>>  )) (native-fun scm-to-int8    ))
(define-method (to-type (target <meta<usint>>) (source <meta<obj>>  )) (native-fun scm-to-uint16  ))
(define-method (to-type (target <meta<sint>> ) (source <meta<obj>>  )) (native-fun scm-to-int16   ))
(define-method (to-type (target <meta<uint>> ) (source <meta<obj>>  )) (native-fun scm-to-uint32  ))
(define-method (to-type (target <meta<int>>  ) (source <meta<obj>>  )) (native-fun scm-to-int32   ))
(define-method (to-type (target <meta<ulong>>) (source <meta<obj>>  )) (native-fun scm-to-uint64  ))
(define-method (to-type (target <meta<long>> ) (source <meta<obj>>  )) (native-fun scm-to-int64   ))
(define-method (to-type (target <meta<int<>>>) (source <meta<int<>>>)) (functional-code mov       ))
(define-method (to-type (target <meta<int<>>>) (source <meta<bool>> )) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<bool>> )) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<int<>>>)) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<obj>>  )) (native-fun scm-to-bool    ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<obj>>  )) (functional-code mov       ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<ubyte>>)) (native-fun scm-from-uint8 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<byte>> )) (native-fun scm-from-int8  ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<usint>>)) (native-fun scm-from-uint16))
(define-method (to-type (target <meta<obj>>  ) (source <meta<sint>> )) (native-fun scm-from-int16 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<uint>> )) (native-fun scm-from-uint32))
(define-method (to-type (target <meta<obj>>  ) (source <meta<int>>  )) (native-fun scm-from-int32 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<ulong>>)) (native-fun scm-from-uint64))
(define-method (to-type (target <meta<obj>>  ) (source <meta<long>> )) (native-fun scm-from-int64 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<bool>> )) (native-fun obj-from-bool  ))
(define-method (to-type (target <meta<composite>>) (source <meta<composite>>))
  (lambda (out args)
    (append-map
      (lambda (channel) (code (channel (delegate out)) (channel (delegate (car args)))))
      (components source))))

(define-method (to-type (target <meta<element>>) (a <param>))
  (let [(to-target  (cut to-type target <>))
        (coercion   (cut convert-type target <>))]
    (make-function to-target coercion (delegate-fun to-target) (list a))))
(define-method (to-type (target <meta<element>>) (self <element>))
  (let [(f (jit ctx (list (class-of self)) (cut to-type target <>)))]
    (add-method! to-type
                 (make <method>
                       #:specializers (map class-of (list target self))
                       #:procedure (lambda (target self) (f (get self)))))
    (to-type target self)))

(define (ensure-default-strides img)
  "Create a duplicate of the array unless it is compact"
  (if (equal? (strides img) (default-strides (shape img))) img (duplicate img)))

(define-syntax-rule (pass-parameters parameters body ...)
  (let [(first-six-parameters (take-up-to parameters 6))
        (remaining-parameters (drop-up-to parameters 6))]
    (append (map (lambda (register parameter)
                   (MOV (to-type (native-equivalent (type parameter)) register) (get (delegate parameter))))
                 register-parameters
                 first-six-parameters)
            (map (lambda (parameter) (PUSH (get (delegate parameter)))) remaining-parameters)
            (list body ...)
            (list (ADD RSP (* 8 (length remaining-parameters)))))))

(define* ((native-fun native) out args)
  (force-parameters (argument-types native) args call-needs-intermediate?
    (lambda intermediates
      (blocked caller-saved
        (pass-parameters intermediates
          (MOV RAX (function-pointer native))
          (CALL RAX)
          (MOV (get (delegate out)) (to-type (native-equivalent (return-type native)) RAX)))))))

(define (make-native-function native . args)
  (make-function make-native-function (const (return-type native)) (native-fun native) args))

(define (native-call return-type argument-types function-pointer)
  (cut make-native-function (make-native-method return-type argument-types function-pointer) <...>))

(define* ((native-data native) out args) (list (MOV (get (delegate out)) (get native))))

(define (make-constant-function native . args) (make-function make-constant-function (const (return-type native)) (native-data native) args))

(define (native-const type value) (make-constant-function (native-value type value)))

; Scheme list manipulation
(define main (dynamic-link))
(define scm-eol (native-const <obj> (scm->address '())))
(define scm-cons (native-call <obj> (list <obj> <obj>) (dynamic-func "scm_cons" main)))
(define scm-gc-malloc-pointerless (native-call <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc_pointerless" main)))
(define scm-gc-malloc             (native-call <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc"             main)))
