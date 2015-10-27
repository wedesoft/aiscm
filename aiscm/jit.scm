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
  #:use-module (aiscm rgb)
  #:use-module (aiscm sequence)
  #:export (<block> <cmd> <var> <ptr>
            substitute-variables variables get-args input output labels next-indices live-analysis
            callee-saved save-registers load-registers blocked repeat
            spill-variable save-and-use-registers register-allocate spill-blocked-predefines
            virtual-variables flatten-code relabel
            idle-live fetch-parameters spill-parameters
            filter-blocks blocked-intervals
            fragment type var decompose var vars skel mov-part
            <pointer<rgb<>>> <meta<pointer<rgb<>>>>
            <fragment<top>> <meta<fragment<top>>>
            <fragment<element>> <meta<fragment<element>>>
            <fragment<rgb<>>> <meta<fragment<rgb<>>>>
            <fragment<pointer<>>> <meta<fragment<pointer<>>>>
            <fragment<sequence<>>> <meta<fragment<sequence<>>>>
            parameter code get-op get-name to-type type assemble jit
            ~ & | ^ << >> =0 !=0 != && || %))
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

(define-syntax-rule (mutable-op op)
  (define-method (op . args) (make <cmd> #:op op #:io (list (car args)) #:in (cdr args))))
(define-syntax-rule (immutable-op op)
  (define-method (op . args) (make <cmd> #:op op #:out (list (car args)) #:in (cdr args))))
(define-syntax-rule (state-setting-op op)
  (define-method (op . args) (make <cmd> #:op op #:in args)))
(define-syntax-rule (state-reading-op op)
  (define-method (op . args) (make <cmd> #:op op #:out args)))

(immutable-op     MOV)
(immutable-op     MOVSX)
(immutable-op     MOVZX)
(immutable-op     LEA)
(mutable-op       SHL)
(mutable-op       SHR)
(mutable-op       SAL)
(mutable-op       SAR)
(mutable-op       ADD)
(state-setting-op PUSH)
(state-reading-op POP)
(mutable-op       NOT)
(mutable-op       NEG)
(mutable-op       INC)
(mutable-op       SUB)
(mutable-op       IMUL)
(mutable-op       IDIV)
(mutable-op       DIV)
(mutable-op       AND)
(mutable-op       OR)
(mutable-op       XOR)
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

(define-method (mov-part (r <register>) (r/m <register>))
   (MOV r (reg (/ (get-bits r) 8) (get-code r/m))))
(immutable-op mov-part)

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

(define (adjacent intervals var) ((overlap intervals) var))

(define (spill-candidate prog live lst) (argmax (idle-live prog live) lst))

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
         (colors     (color-intervals intervals
                                      vars
                                      registers
                                      #:predefined predefined
                                      #:blocked blocked))
         (unassigned (find (compose not cdr) (reverse colors)))]
    (if unassigned
      (let* [(var          (spill-candidate prog live (adjacent intervals (car unassigned))))
             (stack-param? (and (index var parameters) (>= (index var parameters) 6)))
             (location     (if stack-param?
                               (ptr (typecode var) RSP (* 8 (- (index var parameters) 5)))
                               (ptr (typecode var) RSP offset)))]
        (with-spilled-variable var location prog predefined blocked
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
        (let* [(spill-candidate (car conflict))
               (location        (ptr (typecode spill-candidate) RSP offset))]
        (with-spilled-variable spill-candidate location prog predefined blocked
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
(define ((binary-cmp set1 set2) r a b)
  (list (CMP a b) ((if (signed? (typecode a)) set1 set2) r)))
(define ((binary-bool op) r a b)
  (let [(r1 (var <byte>))
        (r2 (var <byte>))]
    (list (TEST a a) (SETNE r1) (TEST b b) (SETNE r2) (op r1 r2) (MOV r r1))))
(define (expand reg) (case (get-bits reg) ((8) (CBW)) ((16) (CWD)) ((32) (CDQ)) ((64) (CQO))))
(define (div/mod r a b pick)
  (let* [(size   (size-of r))
         (ax     (reg size 0))
         (dx     (reg size 2))
         (result (pick (cons ax dx)))]
    (blocked RAX
      (if (signed? (typecode r))
        (if (= size 1)
          (list (MOV ax a) (expand ax) (IDIV b) (blocked RDX (MOV DL AH) (MOV r result)))
          (list (MOV ax a) (blocked RDX (expand ax) (IDIV b) (MOV r result))))
        (if (= size 1)
          (list (MOVZX AX a) (DIV b) (blocked RDX (MOV DL AH) (MOV r result)))
          (list (MOV ax a) (blocked RDX (MOV dx 0) (DIV b) (MOV r result))))))))
(define (div r a b) (div/mod r a b car))
(define (mod r a b) (div/mod r a b cdr))
(define (sign-space a b)
  (let [(coerced (coerce a b))]
    (if (eqv? (signed? (typecode a)) (signed? (typecode b)))
      coerced
      (to-type (integer (min 64 (* 2 (bits (typecode coerced)))) signed) coerced))))
(define (shl r x) (blocked RCX (mov-part CL x) ((if (signed? (typecode r)) SAL SHL) r CL)))
(define (shr r x) (blocked RCX (mov-part CL x) ((if (signed? (typecode r)) SAR SHR) r CL)))
(define-class* <fragment<top>> <object> <meta<fragment<top>>> <class>
              (name  #:init-keyword #:name  #:getter get-name)
              (args  #:init-keyword #:args  #:getter get-args)
              (code  #:init-keyword #:code  #:getter code)
              (value #:init-keyword #:value #:getter get-value))
(define-generic type)
(define (fragment t)
  (template-class (fragment t) (fragment (super t))
    (lambda (class metaclass)
      (define-method (type (self metaclass)) t)
      (define-method (type (self class)) t))))
(fragment <element>)
(define-method (parameter self)
  (make (fragment (class-of self)) #:args (list self) #:name parameter #:code '() #:value (get-value self)))
(define-method (parameter (p <pointer<>>))
  (let [(result (basic (typecode p)))]
    (make (fragment (typecode p))
          #:args (list p)
          #:name parameter
          #:code (list (MOV result (ptr (typecode p) (get-value p))))
          #:value result)))
(pointer <rgb<>>)
(define-method (parameter (p <pointer<rgb<>>>))
  (let [(result (basic (typecode p)))
        (size   (size-of (base (typecode p))))]
    (make (fragment (typecode p))
          #:args (list p)
          #:name parameter
          #:code (list (MOV (red   result) (ptr (base (typecode p)) (get-value p)           ))
                       (MOV (green result) (ptr (base (typecode p)) (get-value p)      size))
                       (MOV (blue  result) (ptr (base (typecode p)) (get-value p) (* 2 size))))
          #:value result)))
(define-method (parameter (self <sequence<>>))
  (make (fragment (class-of self)) #:args (list self) #:name parameter #:code '() #:value self))
(define-method (to-type (target <meta<element>>) (self <meta<element>>))
  target)
(define-method (to-type (target <meta<element>>) (self <meta<sequence<>>>))
  (multiarray target (dimension self)))
(define-method (to-type (target <meta<element>>) (frag <fragment<element>>))
  (let* [(source (typecode (type frag)))
         (result (var target))
         (mov    (if (>= (size-of source) (size-of target))
                     mov-part
                     (if (signed? source)
                         MOVSX
                         (if (>= (size-of source) 4) MOV MOVZX))))]
    (make (fragment (to-type target (type frag)))
          #:args (list target frag)
          #:name to-type
          #:code (append (code frag) (list (mov result (get-value frag))))
          #:value result)))
(fragment <rgb<>>)
(define-method (to-type (target <meta<rgb<>>>) (frag <fragment<element>>))
  (let* [(tmp    (parameter (make (type frag) #:value (get-value frag))))
         (r      (to-type (base target) (red   tmp)))
         (g      (to-type (base target) (green tmp)))
         (b      (to-type (base target) (blue  tmp)))]
    (make (fragment (to-type target (type frag)))
          #:args (list target frag)
          #:name to-type
          #:code (append (code frag) (code r) (code g) (code b))
          #:value (rgb (get-value r) (get-value g) (get-value b)))))
(fragment <pointer<>>)
(fragment <sequence<>>)
(define (mutable-unary op result a)
  (append (code a) (list (MOV result (get-value a)) (op result))))
(define (immutable-unary op result a)
  (append (code a) (list (op result (get-value a)))))
(define-syntax-rule (unary-op name mode op conversion)
  (define-method (name (a <fragment<element>>))
    (let* [(target (conversion (type a)))
           (result (basic (typecode target)))]
      (make (fragment target)
            #:args (list a)
            #:name name
            #:code (mode op result a)
            #:value result))))
(unary-op - mutable-unary NEG identity)
(unary-op ~ mutable-unary NOT identity)
(unary-op =0 immutable-unary (lambda (r a) (list (TEST a a) (SETE r))) (cut to-type <bool> <>))
(unary-op !=0 immutable-unary (lambda (r a) (list (TEST a a) (SETNE r))) (cut to-type <bool> <>))
; TODO: unary operation conj
; TODO: unary operation abs (scalar)
; TODO: unary operation arg (float-scalar)
; TODO: unary operation floor
; TODO: unary operation ceil
; TODO: unary operation round
(define (mutable-binary op result intermediate a b)
  (let [(a~  (to-type intermediate a))
        (b~  (to-type intermediate b))
        (tmp (skel intermediate))]
    (append (code a~) (code b~)
            (list (MOV result          (get-value a~))
                  (MOV (get-value tmp) (get-value b~))
                  (op result (get-value tmp))))))
(define (immutable-binary op result intermediate a b)
  (let [(a~ (to-type intermediate a))
        (b~ (to-type intermediate b))]
    (append (code a~) (code b~)
            (list (op result (get-value a~) (get-value b~))))))
(define (shift-binary op result intermediate a b)
  (append (code a) (code b) (list (MOV result (get-value a)) (op result (get-value b)))))
(define-method (protect self fun) fun); TODO: refactor
(define-method (protect (self <meta<sequence<>>>) fun) list)
(define-syntax-rule (binary-op name mode coercion op conversion)
  (define-method (name (a <fragment<element>>) (b <fragment<element>>))
    (let* [(intermediate (coercion (type a) (type b)))
           (target       (conversion intermediate))
           (result       (basic (typecode target)))]
      (make (fragment target)
            #:args (list a b)
            #:name name
            #:code ((protect intermediate mode) op result intermediate a b)
            #:value result))))
(binary-op +  mutable-binary   coerce     ADD  identity)
(binary-op -  mutable-binary   coerce     SUB  identity)
(binary-op *  mutable-binary   coerce     IMUL identity)
(binary-op &  mutable-binary   coerce     AND  identity)
(binary-op |  mutable-binary   coerce     OR   identity)
(binary-op ^  mutable-binary   coerce     XOR  identity)
(binary-op << shift-binary     coerce     shl  identity)
(binary-op >> shift-binary     coerce     shr  identity)
(binary-op /  immutable-binary coerce     div  identity)
(binary-op %  immutable-binary coerce     mod  identity)
(binary-op =  immutable-binary coerce     (binary-cmp SETE SETE)     (cut to-type <bool> <>))
(binary-op != immutable-binary coerce     (binary-cmp SETNE SETNE)   (cut to-type <bool> <>))
(binary-op <  immutable-binary sign-space (binary-cmp SETL SETB)     (cut to-type <bool> <>))
(binary-op <= immutable-binary sign-space (binary-cmp SETLE SETBE)   (cut to-type <bool> <>))
(binary-op >  immutable-binary sign-space (binary-cmp SETNLE SETNBE) (cut to-type <bool> <>))
(binary-op >= immutable-binary sign-space (binary-cmp SETNL SETNB)   (cut to-type <bool> <>))
(binary-op && immutable-binary coerce     (binary-bool AND)          (cut to-type <bool> <>))
(binary-op || immutable-binary coerce     (binary-bool OR)           (cut to-type <bool> <>))
; TODO: binary operation ** (coercion-maxint)
; TODO: conditional -> minor, major
(define (do-unary-rgb-op op a)
  (let* [(u (parameter (make (type a) #:value (get-value a))))
         (x (op (red   u)))
         (y (op (green u)))
         (z (op (blue  u)))]
  (make (fragment (type a))
        #:args (list a)
        #:name op
        #:code (append (code a) (code x) (code y) (code z))
        #:value (rgb (get-value x) (get-value y) (get-value z)))))
(define-syntax-rule (unary-rgb-op op)
  (define-method (op (a <fragment<rgb<>>>))
    (do-unary-rgb-op op a)))
(unary-rgb-op -)
(unary-rgb-op ~)
; TODO: RGB =0
; TODO: RGB !=0
; TODO: RGB floor
; TODO: RGB ceil
; TODO: RGB round
(define (do-binary-rgb-op op a b)
  (let* [(target (coerce (type a) (type b)))
         (u   (parameter (make (type a) #:value (get-value a))))
         (v   (parameter (make (type b) #:value (get-value b))))
         (x   (op (red   u) (red   v)))
         (y   (op (green u) (green v)))
         (z   (op (blue  u) (blue  v)))]
    (make (fragment target)
          #:args (list a b)
          #:name op
          #:code (append (code a) (code b)
                         (code x) (code y) (code z))
          #:value (rgb (get-value x) (get-value y) (get-value z)))))
(define-syntax-rule (binary-rgb-op op)
  (begin
    (define-method (op (a <fragment<rgb<>>>) (b <fragment<rgb<>>>))
      (do-binary-rgb-op op a b))
    (define-method (op (a <fragment<rgb<>>>) (b <fragment<element>>))
      (do-binary-rgb-op op a b))
    (define-method (op (a <fragment<element>>) (b <fragment<rgb<>>>))
      (do-binary-rgb-op op a b))) )
(binary-rgb-op +)
(binary-rgb-op -)
(binary-rgb-op *)
(binary-rgb-op &)
(binary-rgb-op |)
(binary-rgb-op ^)
(binary-rgb-op <<)
(binary-rgb-op >>)
(binary-rgb-op /)
(binary-rgb-op %)
;TODO: RGB =
;TODO: RGB !=
;TODO: RGB <
;TODO: RGB <=
;TODO: RGB >
;TODO: RGB >=
;TODO: RGB &&
;TODO: RGB ||
;TODO: RGB ** (coercion-maxint)
;TODO: RGB minor, major
(define (var type) (make <var> #:type type))
(define (vars type) (map var (types type)))
(define-method (decompose (self <element>)) (decompose (get-value self)))
(define-method (decompose (self <var>)) (list self)); TODO: decompose for var still needed?
(define-method (decompose (self <rgb>)) (list (red self) (green self) (blue self)))
(define-method (decompose (self <sequence<>>))
  (append (map last (list (shape self) (strides self))) (decompose (project self))))
(define-method (skel (self <meta<element>>)) (make self #:value (car (vars self))))
(define-method (skel (self <meta<rgb<>>>)) (make self #:value (apply rgb (vars self))))
(define-method (skel (self <meta<sequence<>>>))
  (let [(slice (skel (project self)))]
    (make self
          #:value   (get-value slice)
          #:shape   (cons (var <long>) (shape   slice))
          #:strides (cons (var <long>) (strides slice)))))
(define (basic self) (get (skel self))); TOOD: rename
(define-method (project self) self)
(define-method (project (self <fragment<sequence<>>>))
  (apply (get-name self) (map project (get-args self))))
(define-method (store (a <element>) (b <fragment<element>>))
  (append (code b) (list (MOV (get-value a) (get-value b)))))
(define-method (protect (self <fragment<sequence<>>>) fun) list)
(define (component self name)
  (make (fragment (base (type self)))
          #:args (list self)
          #:name name
          #:code (code self)
          #:value ((protect self name) (get-value self))))
(define-method (red   (self <fragment<element>>)) (component self red  ))
(define-method (green (self <fragment<element>>)) (component self green))
(define-method (blue  (self <fragment<element>>)) (component self blue ))
(define-method (store (p <pointer<>>) (a <fragment<element>>))
  (append (code a) (list (MOV (ptr (typecode p) (get-value p)) (get-value a)))))
(define-method (store (p <pointer<rgb<>>>) (a <fragment<rgb<>>>))
  (let [(size (size-of (base (typecode p))))]
    (append (code a)
            (list (MOV (ptr (base (typecode p)) (get-value p)           ) (red   (get-value a)))
                  (MOV (ptr (base (typecode p)) (get-value p)       size) (green (get-value a)))
                  (MOV (ptr (base (typecode p)) (get-value p) (* 2 size)) (blue  (get-value a)))))))
(define-class <elementwise> ()
  (setup     #:init-keyword #:setup     #:getter get-setup)
  (increment #:init-keyword #:increment #:getter get-increment)
  (body      #:init-keyword #:body      #:getter get-body))
(define-method (element-wise self)
  (make <elementwise> #:setup '() #:increment '() #:body self))
(define-method (element-wise (s <sequence<>>))
  (let [(incr (basic <long>))
        (p    (basic <long>))]
    (make <elementwise>
          #:setup (list (IMUL incr (last (strides s)) (size-of (typecode s)))
                        (MOV p (get-value s)))
          #:increment (list (ADD p incr))
          #:body (project (rebase p s)))))
(define-method (element-wise (self <fragment<sequence<>>>))
  (let [(loops (map element-wise (get-args self)))]
    (make <elementwise>
          #:setup (map get-setup loops)
          #:increment (map get-increment loops)
          #:body (apply (get-name self) (map get-body loops)))))
(define-method (store (s <sequence<>>) (a <fragment<sequence<>>>))
  (let [(destination (element-wise s))
        (source      (element-wise a))]
    (list (get-setup destination)
          (get-setup source)
          (repeat (last (shape s))
                  (append (store (get-body destination) (get-body source))
                          (get-increment destination)
                          (get-increment source))))))

(define-method (returnable? self) #f)
(define-method (returnable? (self <meta<bool>>)) #t)
(define-method (returnable? (self <meta<int<>>>)) #t)
(define (assemble retval vars frag)
  (virtual-variables (if (returnable? (class-of retval)) (list (get retval)) '())
                     (concatenate (map decompose (if (returnable? (class-of retval)) vars (cons retval vars))))
                     (append (store retval frag) (list (RET)))))
(define (jit ctx classes proc)
  (let* [(vars        (map skel classes))
         (frag        (apply proc (map parameter vars)))
         (result-type (type frag))
         (return?     (returnable? result-type))
         (target      (if return? result-type (pointer result-type)))
         (code        (asm ctx
                           (if return? (car (types target)) <null>)
                           (concatenate (map types (if return? classes (cons target classes))))
                           (assemble (skel target) vars frag)))
         (fun         (lambda header (apply code (concatenate (map content header)))))]
    (if return?
        (lambda args
           (let [(result (apply fun args))]
             (get (build result-type result))))
        (lambda args
          (let [(result (make target #:shape (argmax length (map shape args))))]
            (apply fun (cons result args))
            (get (build result-type result)))))))
