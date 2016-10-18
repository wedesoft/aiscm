(use-modules (oop goops)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (aiscm util)
             (aiscm asm)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm bool)
             (aiscm rgb)
             (aiscm obj)
             (aiscm complex)
             (guile-tap))
(define ctx (make <context>))
(ok (not (native-equivalent (rgb 1 2 3)))
    "RGB does not have a native equivalent")
(ok (eq? <sint> (native-equivalent <sint>))
    "short integer is it's own native equivalent")
(ok (eq? <ubyte> (native-equivalent <bool>))
    "byte is native equivalent of boolean")
(ok (eq? <ulong> (native-equivalent (pointer <ubyte>)))
    "native equivalent of pointer is a 64 bit integer")
(ok (eq? <ulong> (native-equivalent <obj>))
    "native equivalent of Scheme reference is a 64 bit integer")
(let [(a (var <int>))
      (b (var <int>))]
  (ok (equal? (list b) (input (MOV a b)))
      "Get input variables of MOV")
  (ok (equal? (list a b) (input (ADD a b)))
      "Get input variables of ADD")
  (ok (equal? (list a) (input (ADD a a)))
      "Prevent duplication of input variables")
  (ok (equal? (list a) (output (MOV a b)))
      "Get output variables of MOV")
  (ok (equal? (list a) (output (ADD a b)))
      "Get output variables of ADD")
  (ok (equal? (list b a) (input (MOV (ptr <int> a) b)))
    "Get input variables of command writing to address")
  (ok (equal? (list a 0) (get-args (MOV a 0)))
      "Get arguments of command")
  (ok (equal?  (list a b) (variables (list (MOV a 0) (MOV b a))))
      "Get variables of a program")
  (let [(p (var <long>))]
    (ok (equal?  (list a p) (variables (list (MOV a 0) (MOV (ptr <int> p) a))))
        "Get variables of a program using a pointer"))
  (ok (equal? (list (MOV ECX 42)) (substitute-variables (list (MOV a 42)) (list (cons a RCX))))
      "Substitute integer variable with register")
  (ok (equal? (MOV EAX 0)
              (substitute-variables (substitute-variables (MOV a 0) (list (cons a b))) (list (cons b RAX))))
      "Substitute variable with another")
  (ok (equal? (MOV ECX EDX) (substitute-variables (MOV a b) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'MOV'")
  (let [(p (var <long>))]
    (ok (equal? (list (MOV RCX (ptr <long> RAX)))
                (substitute-variables (list (MOV p (ptr <long> RAX))) (list (cons p RCX))))
        "Substitute long integer variable with register")
    (ok (equal? (ptr <int> RCX) (substitute-variables (ptr <int> p) (list (cons p RCX))))
        "Substitute pointer variable with register")
    (ok (equal? (ptr <int> p 2) (substitute-variables (ptr <int> p 2) '()))
        "Pass through pointer with empty substitution")
    (ok (equal? (ptr <int> RCX 5)
                (substitute-variables (ptr <int> p 2) (list (cons p (cons RCX 3)))))
        "Substitute pointer variable with register and offset"))
  (let [(l (var <long>))
        (w (var <sint>))]
    (ok (equal? (MOVSX RCX EDX) (substitute-variables (MOVSX l a) (list (cons l RCX) (cons a RDX))))
        "Substitution works with 'MOVSX'")
    (ok (equal? (MOVZX ECX DX) (substitute-variables (MOVZX a w) (list (cons a RCX) (cons w RDX))))
        "Substitution works with 'MOVZX'"))
  (let [(p (var <long>))
        (q (var <long>))]
    (ok (equal? (LEA RCX (ptr <byte> RDX))
                (substitute-variables (LEA p (ptr <byte> q)) (list (cons p RCX) (cons q RDX))))
        "Substitution works with 'LEA"))
  (ok (equal? (SHL ECX) (substitute-variables (SHL a) (list (cons a RCX))))
      "Substitution works with 'SHL")
  (ok (equal? (SHR ECX) (substitute-variables (SHR a) (list (cons a RCX))))
      "Substitution works with 'SHR")
  (ok (equal? (SAL ECX) (substitute-variables (SAL a) (list (cons a RCX))))
      "Substitution works with 'SAL")
  (ok (equal? (SAR ECX) (substitute-variables (SAR a) (list (cons a RCX))))
      "Substitution works with 'SAR")
  (ok (equal? (ADD ECX EDX) (substitute-variables (ADD a b) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'ADD'")
  (ok (equal? (PUSH ECX) (substitute-variables (PUSH a) (list (cons a RCX))))
      "Substitution works with 'PUSH'")
  (ok (equal? (POP ECX) (substitute-variables (POP a) (list (cons a RCX))))
      "Substitution works with 'POP'")
  (ok (equal? (NEG ECX) (substitute-variables (NEG a) (list (cons a RCX))))
      "Substitution works with 'NEG'")
  (ok (equal? (SUB ECX EDX) (substitute-variables (SUB a b) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'SUB'")
  (ok (equal? (IMUL ECX EDX) (substitute-variables (IMUL a b) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'IMUL'")
  (ok (equal? (IMUL ECX EDX 2) (substitute-variables (IMUL a b 2) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'IMUL' and three arguments")
  (ok (equal? (CMP ECX EDX) (substitute-variables (CMP a b) (list (cons a RCX) (cons b RDX))))
      "Substitution works with 'CMP'")
  (let [(u (var <ubyte>))]
    (ok (equal? (SETB CL) (substitute-variables (SETB u) (list (cons u RCX))))
        "Substitution works with 'SETB'"))
  (ok (equal? '((a . 1) (b . 3)) (labels (list (JMP 'a) 'a (MOV AX 0) 'b (RET))))
      "'labels' should extract indices of labels"))
(ok (equal? (MOV AX CX) (mov-signed AX CX))
    "copy signed 16-bit value")
(ok (equal? (MOVSX EAX CX) (mov-signed EAX CX))
    "copy with sign extension")
(ok (equal? (MOV CL SIL) (mov-signed CL ESI))
    "copy part of signed value")
(ok (equal? (MOV AX CX) (mov-unsigned AX CX))
    "copy unsigned 16-bit value")
(ok (equal? (MOVZX EAX CX) (mov-unsigned EAX CX))
    "copy with zero extension")
(ok (equal? (MOV CL SIL) (mov-unsigned CL ESI))
    "copy part of unsigned value")
(ok (equal? (MOV RAX RCX) (mov-unsigned RAX ECX)); TODO: map to MOV EAX ECX
    "zero-extending 32-bit value is done by default")
(ok (equal? '(1) (next-indices (MOV CX 0) 0 '()))
    "Get following indices for first statement in a program")
(ok (equal? '(2) (next-indices (MOV AX CX) 1 '()))
    "Get following indices for second statement in a program")
(ok (equal? '() (next-indices (RET) 2 '()))
    "RET statement should not have any following indices")
(ok (equal? '(2) (next-indices (JMP 'a) 0 '((a . 2))))
    "Get following indices for a jump statement")
(ok (equal? '(1 2) (next-indices (JNE 'a) 0 '((a . 2))))
    "Get following indices for a conditional jump")
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (ok (equal? (list '() (list a) '()) (live-analysis (list 'x (MOV a 0) (RET))))
      "Live-analysis for definition of unused variable")
  (ok (equal? (list (list a) (list a) (list b a) '()) (live-analysis (list (MOV a 0) (NOP) (MOV b a) (RET))))
      "Live-analysis for definition and later use of a variable")
  (ok (equal? (list (list a) (list a) (list a) (list a) '())
              (live-analysis (list (MOV a 0) 'x (ADD a 1) (JE 'x) (RET))))
      "Live-analysis with conditional jump statement")
  (ok (equal? (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET))
              (register-allocate (list (MOV a 42) (RET))))
      "Allocate a single register")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
              (register-allocate (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
      "Allocate a single register using custom list of registers")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 1) (MOV EAX 2) (ADD ECX EAX) (MOV EAX ECX) (ADD RSP 8) (RET))
              (register-allocate (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
      "Allocate multiple registers")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 1) (ADD ECX ESI) (MOV EAX ECX) (ADD RSP 8) (RET))
              (register-allocate (list (MOV b 1) (ADD b a) (MOV c b) (RET))
                                 #:predefined (list (cons a RSI) (cons c RAX))))
      "Register allocation with predefined registers")
  (ok (equal? (list (SUB RSP 8) (MOV EAX EDI) (ADD EAX ESI) (ADD RSP 8) (RET))
              (virtual-variables (list a) (list b c) (list (MOV a b) (ADD a c) (RET))))
      "'virtual-variables' uses the specified variables as parameters")
  (ok (equal? (list (SUB RSP 8) (MOV ECX EDI) (MOV EAX ECX) (ADD RSP 8) (RET))
              (virtual-variables (list a) (list b) (list (MOV c b) (MOV a c) (RET))))
      "'virtual-variables' allocates local variables"))
(ok (eq? 'new (get-target (retarget (JMP 'old) 'new)))
    "'retarget' should update target of jump statement")
(ok (equal? (list (JMP 1) 'a (NOP) (RET))
            (flatten-code (list (list (JMP 1) 'a) (NOP) (RET))))
    "'flatten-code' should flatten nested environments")
(let [(a (var <int>))
      (b (var <int>))]
  (ok (equal? (list (SUB RSP 8) (MOV EAX EDI) (ADD RSP 8) (RET))
              (virtual-variables (list a) (list b) (list (list (MOV a b)) (RET))))
      "'pass-parameter-variables' handles nested code blocks")
  (ok (equal? (list (SUB RSP 8) (MOV ECX (ptr <int> RSP 16)) (MOV EAX ECX) (ADD RSP 8) (RET))
              (let [(args (map var (make-list 7 <int>)))]
                 (virtual-variables (list a) args (list (MOV a (last args)) (RET)))))
      "'virtual-variables' maps the 7th integer parameter correctly"))
(ok (equal? (resolve-jumps (list (JMP 'b) (JMP 'a) 'a (NOP) 'b))
            (resolve-jumps (flatten-code (relabel (list (JMP 'a) (list (JMP 'a) 'a) (NOP) 'a)))))
    "'relabel' should create separate namespaces for labels")
(ok (lset= eq? (list RBP R12) (callee-saved (list RAX RBP R10 R10 R12 R12)))
    "'callee-saved' should extract the set of callee-saved registers")
(ok (equal? (list (MOV (ptr <long> stack-pointer #x-8) RBP) (MOV (ptr <long> stack-pointer #x-10) R12))
            (save-registers (list RBP R12) #x-8))
    "'save-registers' should generate instructions for saving registers on the stack")
(ok (equal? (list (MOV (ptr <long> stack-pointer #x-10) RBP) (MOV (ptr <long> stack-pointer #x-18) R12))
            (save-registers (list RBP R12) #x-10))
    "'save-registers' should use the specified offset")
(ok (equal? (list (MOV RBP (ptr <long> stack-pointer #x-8)) (MOV R12 (ptr <long> stack-pointer #x-10)))
            (load-registers (list RBP R12) #x-8))
    "'load-registers' should generate instructions for saving registers on the stack")
(ok (equal? (list (MOV RBP (ptr <long> stack-pointer #x-10)) (MOV R12 (ptr <long> stack-pointer #x-18)))
            (load-registers (list RBP R12) #x-10))
    "'load-registers' should use the specified offset")
(let [(a (var <int>))]
  (ok (equal? (list (MOV (ptr <long> stack-pointer #x-8) R12) (MOV R12D 0) (MOV R12 (ptr <long> stack-pointer #x-8)) (RET))
              (save-and-use-registers (list (MOV a 0) (RET)) (list (cons a R12)) '() -8))
      "'save-and-use-registers' should save and restore callee-saved registers")
  (ok (equal? (list (MOV (ptr <long> stack-pointer #x-10) R12) (MOV R12D 0) (MOV R12 (ptr <long> stack-pointer #x-10)) (RET))
              (save-and-use-registers (list (MOV a 0) (RET)) (list (cons a R12)) '() -16))
      "'save-and-use-registers' should use the specified offset for saving callee-saved registers")
  (ok (eqv? 3 ((asm ctx <int> '()
                    (virtual-variables (list a) '()
                                       (list (MOV a 0) (JMP 'a) (list 'a (MOV a 2)) 'a (ADD a 3) (RET)))) ))
      "'virtual-variables' creates separate namespaces for labels")
  (ok (equal? (list (MOV EAX 42) 'x (RET))
              (flatten-code (spill-variable a (ptr <int> RSP -8) (list (MOV EAX 42) 'x (RET)))))
      "Variable spilling ignores machine code and labels")
  (ok (equal? (list (SUB RSP 8) (MOV EAX 0) (MOV (ptr <int> RSP -8) EAX) (ADD RSP 8) (RET))
              (register-allocate (flatten-code (spill-variable a (ptr <int> RSP -8) (list (MOV a 0) (RET))))))
      "Write spilled variable to stack")
  (ok (equal? (list (SUB RSP 8) (MOV EAX (ptr <int> RSP -16)) (MOV ECX EAX) (ADD RSP 8) (RET))
              (register-allocate (flatten-code (spill-variable a (ptr <int> RSP -16) (list (MOV ECX a) (RET))))))
      "Read spilled variable from stack"))
(let [(u (var <ubyte>))]
  (ok (equal? (list (SUB RSP 8) (MOV AL (ptr <byte> RSP -24)) (ADD AL 1) (MOV (ptr <byte> RSP -24) AL) (ADD RSP 8) (RET))
              (register-allocate (flatten-code (spill-variable u (ptr <int> RSP -24) (list (ADD u 1) (RET))))))
      "Read and write spilled variable"))
(let [(a (var <int>))
      (b (var <int>))]
  (let  [(p (var <long>))]
    (ok (equal? (let [(prog (list (ADD a b) (ADD a (ptr <int> p)) (RET)))
                      (live (list (list a b p) (list a p) '()))]
                  (map (idle-live prog live) (list a b p)))
                '(0 0 1))
        "Count times a variable is live but not used"))
  (ok (equal? (list (SUB RSP 8) (MOV EDI 1) (MOV ESI 2) (ADD ESI 3) (ADD EDI 4) (ADD RSP 8) (RET))
              (register-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                 #:registers (list RSI RDI)))
      "'register-allocate' should use the specified set of registers")
  (ok (equal? (list (SUB RSP 16)
                    (MOV ESI 1)
                    (MOV (ptr <int> RSP 8) ESI)
                    (MOV ESI 2)
                    (ADD ESI 3)
                    (MOV ESI (ptr <int> RSP 8))
                    (ADD ESI 4)
                    (MOV (ptr <int> RSP 8) ESI)
                    (ADD RSP 16)
                    (RET))
              (register-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                 #:registers (list RSI)))
      "'register-allocate' should spill variables")
  (let  [(c (var <int>))]
    (ok (equal? (list (SUB RSP 24)
                      (MOV ESI 1)
                      (MOV (ptr <int> RSP 16) ESI)
                      (MOV ESI 2)
                      (MOV (ptr <int> RSP 8) ESI)
                      (MOV ESI 3)
                      (ADD ESI 4)
                      (MOV ESI (ptr <int> RSP 8))
                      (ADD ESI 5)
                      (MOV (ptr <int> RSP 8) ESI)
                      (MOV ESI (ptr <int> RSP 16))
                      (ADD ESI 6)
                      (MOV (ptr <int> RSP 16) ESI)
                      (ADD RSP 24)
                      (RET))
                (register-allocate (list (MOV a 1) (MOV b 2) (MOV c 3) (ADD c 4) (ADD b 5) (ADD a 6) (RET))
                                   #:registers (list RSI)))
        "'register-allocate' should assign separate stack locations"))
  (ok (equal? (list (SUB RSP 16)
                    (MOV (ptr <long> RSP 0) RBX)
                    (MOV EBX 1)
                    (MOV (ptr <int> RSP 8) EBX)
                    (MOV EBX 2)
                    (ADD EBX 3)
                    (MOV EBX (ptr <int> RSP 8))
                    (ADD EBX 4)
                    (MOV (ptr <int> RSP 8) EBX)
                    (MOV RBX (ptr <long> RSP 0))
                    (ADD RSP 16)
                    (RET))
              (register-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                 #:registers (list RBX)))
      "'register-allocate' should save callee-saved registers"))
(let [(a (var <int>))]
  (ok (equal? '() ((spill-parameters (list a)) (list (cons a RDI))))
      "Register-parameter does not need spilling if a register is available for it")
  (ok (equal? (list (MOV (ptr <int> RSP -16) EDI))
              ((spill-parameters (list a)) (list (cons a (ptr <int> RSP -16)))))
      "Write spilled parameter to the stack")
  (ok (equal? (list (MOV (ptr <int> stack-pointer -16) EDI))
              ((spill-parameters (list a)) (list (cons a (ptr <int> stack-pointer -16)))))
      "Write spilled parameter with stack pointer variable to the stack")
  (ok (equal? '() ((fetch-parameters (list a)) (list (cons a (ptr <int> RSP +8)))))
      "Stack-parameter does not need loading if it is spilled")
  (ok (equal? (list (MOV R10D (ptr <int> stack-pointer 8)))
              ((fetch-parameters (list a)) (list (cons a R10))))
      "Read prespilled parameters into register if a register is available for it")
  (let [(b (var <int>))
        (c (var <int>))]
    (ok (equal? (list (SUB RSP 16)
                      (MOV (ptr <int> RSP 8) ESI)
                      (MOV ESI EDI)
                      (MOV EDI (ptr <int> RSP 8))
                      (ADD ESI EDI)
                      (ADD RSP 16)
                      (RET))
                (virtual-variables '() (list a b) (list (MOV c a) (ADD c b) (RET)) #:registers (list RSI RDI)))
        "Spill register-parameter to the stack")
    (ok (equal? (list (SUB RSP 8) (MOV EDX 0) (MOV ECX 0) (CMP ECX EAX) (JE #x6) (INC ECX) (INC EDX) (JMP #x-a) (ADD RSP 8) (RET))
                (resolve-jumps (register-allocate (flatten-code (list (MOV a 0) (repeat b (INC a)) (RET))))))
        "'repeat' loop")))
(ok (equal? (list (MOV ECX 2) (RET)) (get-code (blocked AL (MOV ECX 2) (RET))))
    "'blocked' represents the specified code segment")
(ok (equal? RAX (get-reg (blocked RAX (MOV ECX 2) (RET))))
    "'blocked' stores the register to be blocked")
(ok (equal? (list (MOV ECX 2) (RET)) (blocked '() (MOV ECX 2) (RET)))
    "'blocked' with empty block list has no effect")
(ok (equal? (list (MOV ECX 2) (RET)) (filter-blocks (blocked RAX (MOV ECX 2) (RET))))
    "'filter-blocks' should remove blocked-register information")
(ok (equal? (list (MOV EDX 2) 'x (list (RET)))
            (filter-blocks (blocked RDX (MOV EDX 2) 'x (blocked RAX (RET)))))
    "'filter-blocks' should work recursively")
(ok (equal? (list (cons RAX '(0 . 1))) (blocked-intervals (blocked RAX (MOV EAX 0) (RET))))
    "'blocked-intervals' should extract the blocked intervals for each register")
(ok (equal? (list (cons RAX '(1 . 1))) (blocked-intervals (list (MOV EAX 0) (blocked RAX (RET)))))
    "Blocked intervals within a program should be offset correctly")
(ok (equal? (list (cons RAX '(2 . 2))) (blocked-intervals (list (list (MOV EAX 0) (NOP)) (blocked RAX (RET)))))
    "The offsets of 'blocked-intervals' should refer to the flattened code")
(ok (equal? (list (cons RAX '(1 . 4)) (cons RDX '(2 . 3)))
            (blocked-intervals (list 'x (blocked RAX (MOV AX 0) (blocked RDX (MOV DX 0) (IDIV CX)) (RET)))))
    "'blocked-intervals' should work recursively")
(ok (equal? (list (cons RCX '(0 . 1)) (cons RDX '(0 . 1))) (blocked-intervals (blocked (list RCX RDX) (MOV ECX 2) (RET))))
    "'blocked' with list of registers blocks all of them")
(let  [(w (var <usint>))]
  (ok (equal? (list (SUB RSP 8) (MOV AX 0) (ADD RSP 8) (RET))
              (virtual-variables '() '() (list (blocked RCX (MOV w 0)) (RET))))
      "'virtual-variables' filters out the reserved-registers information")
  (ok (equal? (list (SUB RSP 8) (MOV CX 0) (ADD RSP 8) (RET))
              (virtual-variables '() '() (list (blocked RAX (MOV w 0)) (RET))))
      "'virtual-variables' avoids blocked registers when allocating variables"))
(let [(a (var <int>))
      (u (var <ubyte>))]
  (ok (equal? (list (SUB RSP 16)
                    (MOV CL AL)
                    (MOV EAX (ptr <int> RSP 8))
                    (SHL EAX CL)
                    (MOV (ptr <int> RSP 8) EAX)
                    (ADD RSP 16)
                    (RET))
              (spill-blocked-predefines (list (MOV CL u) (SHL a CL) (RET))
                                        #:predefined (list (cons a RCX))
                                        #:blocked (list (cons RCX '(0 . 1)))))
      "Spill predefined registers if they are blocked"))
(ok (eq? <var> (class-of (var <int>)))
    "Shortcut for creating variables creates variables")
(ok (eq? <byte> (typecode (var <byte>)))
    "Shortcut for  creating variables uses specified type")
(ok (eq? <ubyte> (typecode (var <bool>)))
    "Boolean values are represented using unsigned byte")
(let  [(i (skeleton <int>))]
  (ok (is-a? i <int>)
      "skeleton of integer is of type integer")
  (ok (is-a? (value i) <var>)
      "value of integer skeleton is a variable")
  (ok (eq? <int> (typecode (value i)))
      "value of integer skeleton is of type integer"))
(let [(s (skeleton (sequence <byte>)))]
  (ok (is-a? s (sequence <byte>))
      "skeleton of a sequence is a sequence")
  (ok (equal? (list <var> <var> <var>) (map class-of (content (class-of s) s)))
      "sequence skeleton consists of three variables")
  (ok (equal? (list <long> <long> <ulong>) (map typecode (content (class-of s) s)))
      "skeleton of sequence consists of long integer variables"))
(let [(m (skeleton (multiarray <int> 2)))]
  (ok (is-a? m (multiarray <int> 2))
      "skeleton of a 2D array is a 2D array")
  (ok (equal? (make-list 5 <var>) (map class-of (content (class-of m) m)))
      "2D array skeleton consists of five variables")
  (ok (equal? (list <long> <long> <long> <long> <ulong>) (map typecode (content (class-of m) m)))
      "skeleton of 2D array consists of long integer variables"))
(let* [(s  (skeleton (sequence <int>)))
       (sx (parameter s))]
  (ok (eq? (value s) (value (delegate (delegate sx))))
      "sequence parameter maintains pointer")
  (ok (eq? (index sx) (index (delegate sx)))
      "index of parameter and index of parameters content should match")
  (ok (eq? (dimension s) (dimension sx))
      "sequence parameter should maintain dimension")
  (ok (eq? (stride s) (stride (delegate sx)))
      "sequence parameter should maintain stride")
  (ok (eq? (sequence <int>) (type sx))
      "sequence parameter maintains type")
  (let [(i (var <long>))]
    (ok (eq? i (index (subst (delegate sx) (index sx) i)))
        "substitution should replace the lookup index")
    (ok (eq? i (index (get sx i)))
        "retrieving an element by index should replace with the index")))
(let* [(m  (skeleton (multiarray <int> 2)))
       (mx (parameter m))]
  (ok (equal? (shape m) (shape mx))
      "2D array parameter should maintain the shape")
  (ok (equal? (index mx) (index (delegate (delegate mx))))
      "first index of parameter should have a match")
  (ok (equal? (index (delegate mx)) (index (delegate (delegate (delegate mx)))))
      "second index of parameter should have a match")
  (let [(i (var <long>))]
    (ok (eq? i (index (subst (delegate (delegate mx)) (index mx) i)))
      "subst should allow replacing first index")
    (ok (eq? i (index (delegate (subst (delegate (delegate mx)) (index (delegate mx)) i))))
      "subst should allow replacing second index")
    (ok (eq? (index mx) (index (subst (delegate (delegate mx)) (index (delegate mx)) i)))
      "replacing the second index should maintain the first one")
    (ok (eq? i (index (delegate (get mx i))))
      "retrieving an element should replace with the index")))
(let [(a (skeleton <byte>))
      (b (skeleton (pointer <byte>)))
      (c (set-pointer-offset (skeleton (pointer <int>)) 3))]
  (ok (equal? (get a) (operand a))
      "element operand is value of element")
  (ok (equal? (ptr <byte> (get b)) (operand b))
      "pointer operand is pointer to element")
  (ok (equal? (ptr <int> (get c) 3) (operand c))
      "pointer operand can have offset"))
(let [(out (skeleton <int>))
      (in  (skeleton <int>))]
  (ok (equal? (list (list (mov-signed (get out) (get in)))) (code out in))
      "generate code for copying an integer")
  (ok (equal? (list (list (get out)) (list (get in)) (list (list (mov-signed (get out) (get in))) (RET)))
              (assemble (list out) (list in) (code out in) list))
      "generate code for identity function"))
(ok (equal? (list (SUB RSP 8) (MOV RCX RAX) (ADD RSP 8) (RET))
            (register-allocate (attach (code (skeleton <ulong>) (skeleton <uint>)) (RET))))
    "Use default zero-extension for 32-bit numbers")
(ok (eqv? 42 ((jit ctx (list <int>) identity) 42))
    "compile and run integer identity function")
(ok (eqv? #t ((jit ctx (list <bool>) identity) #t))
    "compile and run boolean identity function")
(let [(out (skeleton <int>))
      (in  (skeleton (pointer <int>)))]
  (ok (equal? (list (list (mov-signed (get out) (ptr <int> (get in))))) (code out in))
      "generate code for reading integer from memory"))
(let [(out (skeleton (pointer <int>)))
      (in  (skeleton <int>))]
  (ok (equal? (list (list (mov-signed (ptr <int> (get out)) (get in)))) (code out in))
      "generate code for writing integer to memory"))
(let [(out (skeleton <int>))]
  (ok (equal? (list (MOV (get out) 0)) (code out 0))
      "Generate code for setting variable to zero"))
(let [(out (parameter (sequence <int>)))]
  (ok (eq? (iterator (delegate out)) (iterator out))
      "retrieve iterator pointer from tensor parameter")
  (ok (eq? (step (delegate out)) (step out))
      "retrieve step variable from tensor parameter")
  (ok (not (eq? (step out) (iterator out)))
      "step and iterator need to be distinct variables")
  (ok (is-a? (delegate (project out)) (pointer <int>))
      "projected 1D array tensor should contain pointer"))
(let [(out  (parameter (sequence <int>)))]
  (ok (equal? (list (IMUL (step out) (stride out) (size-of (typecode out)))
                    (MOV (iterator out) (value out)))
              (setup out))
      "setup of array loop should define increment and initialise pointer")
  (ok (equal? (list (ADD (iterator out) (step out))) (increment out))
      "increment of array loop should increment the pointer")
  (ok (equal? (iterator out) (value (body out)))
      "body of loop should be rebased to the pointer")
  (ok (is-a? (delegate (body out)) (pointer <int>))
      "body of array loop should be a pointer object"))
(let [(in  (skeleton (pointer <byte>)))
      (out (skeleton (pointer <byte>)))]
  (ok (equal? (list (SUB RSP 8) (MOV DL (ptr <byte> RCX)) (MOV (ptr <byte> RAX) DL) (ADD RSP 8) (RET))
              (register-allocate (attach (code out in) (RET))))
      "generate code for copying a byte from one memory location to another"))
(ok (equal? '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) identity) (seq <int> 2 3 5))))
    "compile and run identity function for array")
(let* [(i  (var <long>))
       (op (lambda (s) (tensor (dimension s) i (get s i))))]
  (ok (equal? '(2 3 5) (to-list ((jit ctx (list (sequence <int>)) op) (seq <int> 2 3 5))))
      "compile and run trivial 1D tensor function"))
(let [(out (skeleton (multiarray <int> 2)))
      (in  (skeleton (multiarray <int> 2)))]
  (ok (list? (code (parameter out) (parameter in)))
      "generating code for copying a 2D array should run without error"))
(ok (equal? '((2 3 5) (7 9 11))
            (to-list ((jit ctx (list (multiarray <int> 2)) identity) (arr <int> (2 3 5) (7 9 11)))))
    "compile and run identity function for 2D array")
(let [(out (skeleton <int>))
      (a   (skeleton <int>))
      (b   (skeleton <int>))]
  (ok (equal? (list (list (mov-signed (get out) (get a))) (ADD (get out) (get b)))
              (code (parameter out) (+ (parameter a) (parameter b))))
    "generate code for adding two numbers"))
(ok (equal? 42 ((jit ctx (list <int> <int>) +) 19 23))
    "compile and run function adding two numbers")
(let [(out (skeleton <byte>))
      (in  (skeleton <int>))]
  (ok (equal? (list (SUB RSP 8) (list (MOV CL SIL)) (ADD RSP 8) (RET))
              (register-allocate (attach (code out in) (RET))
                                 #:predefined (list (cons (get out) RCX) (cons (get in) RSI))))
      "generate code for copying part of integer (critical case MOV CL SIL)"))
(ok (eq? <int> (type (+ (parameter <usint>) (parameter <byte>))))
  "plus operation coerces return type correctly")
(let [(out (skeleton <int>))
      (a   (skeleton <byte>))
      (b   (skeleton <usint>))]
  (ok (equal? (list (SUB RSP 8) (MOVZX EDX CX) (MOVSX ECX AL) (ADD EDX ECX) (ADD RSP 8) (RET))
              (register-allocate (attach (code (parameter out) (+ (parameter b) (parameter a))) (RET))))
      "sign-extend second number when adding"))
(ok (+ (parameter (sequence <int>)) (parameter <int>))
    "create function from tensor and element")
(ok (+ (parameter <int>) (parameter (sequence <int>)))
    "create function from element and tensor")
(ok (+ (parameter (sequence <int>)) (parameter (sequence <int>)))
    "create function from two tensors")
(let* [(a    (parameter (sequence <int>)))
       (b    (parameter <int>))
       (f    (+ a b))
       (out  (parameter (sequence <int>)))]
  (ok (equal? (list (IMUL (step a) (stride a) (size-of (typecode a)))
                    (MOV (iterator a) (value a)))
              (setup f))
      "setup of loop over array-scalar-function should setup looping over first argument")
  (ok (equal? (list (ADD (iterator a) (step a))) (increment f))
      "loop should increment input array iterator")
  (ok (equal? (iterator a) (value (car (arguments (body f)))))
      "body of loop should be function with element of first argument as argument")
  (ok (equal? b (cadr (arguments (body f))))
      "body of loop should maintain second argument")
  (ok (equal? (list (SUB RSP 8) (MOV ESI (ptr <int> RDX)) (ADD ESI ECX) (MOV (ptr <int> RAX) ESI) (ADD RSP 8) (RET))
              (register-allocate (attach (code (body out) (body f)) (RET))))
      "instantiate loop body for array-scalar-function"))
(let [(out (skeleton (sequence <int>)))
      (a   (skeleton (sequence <int>)))
      (b   (skeleton <int>))]
  (ok (list? (code (parameter out) (+ (parameter a) (parameter b))))
      "generating code for array-scalar operation should run without error"))
(ok (equal? '(9 10 12) (to-list ((jit ctx (list (sequence <int>) <int>) +) (seq <int> 2 3 5) 7)))
    "compile and run array-scalar operation")
(let* [(a    (parameter <int>))
       (b    (parameter (sequence <int>)))
       (f    (+ a b))
       (out  (parameter (sequence <int>)))]
  (ok (equal? (list (IMUL (step b) (stride b) (size-of (typecode b)))
                    (MOV (iterator b) (value b)))
              (setup f))
      "setup of loop over scalar-array-function should setup looping over second argument")
  (ok (equal? a (car (arguments (body f))))
      "body of loop should maintain first argument")
  (ok (equal? (iterator b) (value (cadr (arguments (body f)))))
      "body of loop should be function with element of second argument as argument")
  (ok (equal? (list (SUB RSP 8) (MOV ESI EDX) (ADD ESI (ptr <int> RCX)) (MOV (ptr <int> RAX) ESI) (ADD RSP 8) (RET))
              (register-allocate (attach (code (body out) (body f)) (RET))))
      "instantiate loop body for scalar-array-function"))
(run-tests)
