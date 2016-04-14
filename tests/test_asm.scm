(use-modules (oop goops)
             (aiscm asm)
             (aiscm mem)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (guile-tap))
(define ctx (make <context>))
(define b1 (random (ash 1  6)))
(define b2 (random (ash 1  6)))
(define w1 (random (ash 1 14)))
(define w2 (random (ash 1 14)))
(define i1 (random (ash 1 30)))
(define i2 (random (ash 1 30)))
(define l1 (random (ash 1 62)))
(define l2 (random (ash 1 62)))
(define mem (make <mem> #:size 256))
(define bptr (make (pointer <byte>) #:value mem))
(define wptr (make (pointer <sint>) #:value mem))
(define iptr (make (pointer <int>) #:value mem))
(define lptr (make (pointer <long>) #:value mem))
(define (bdata) (begin
                  (store bptr       b1)
                  (store (+ bptr 1) b2)
                  mem))
(define (wdata) (begin
                  (store wptr       w1)
                  (store (+ wptr 1) w2)
                  mem))
(define (idata) (begin
                  (store iptr       i1)
                  (store (+ iptr 1) i2)
                  mem))
(define (ldata) (begin
                  (store lptr       l1)
                  (store (+ lptr 1) l2)
                  mem))
(define (idx) (begin
                (store lptr #x0102030405060708)
                mem))
(ok (eqv? 4 (get-code RSP))
    "Get code of RSP register")
(ok (eqv? 8 (get-code R8D))
    "Get code of R8D register")
(ok (eqv? 1 (size-of AL))
    "Size of AL")
(ok (eqv? 2 (size-of AX))
    "Size of AX")
(ok (eqv? 4 (size-of EAX))
    "Size of EAX")
(ok (eqv? 8 (size-of RAX))
    "Size of RAX")
(ok (equal? "EAX" (format #f "~a" EAX))
    "Display EAX")
(ok (equal? RSP (reg 8 4))
    "Retrieve RSP by register code and size")
(ok (equal? R8D (reg 4 8))
    "Retrieve R8D by register code and size")
(ok (equal? XMM5 (xmm 5))
    "Retrieve XMM5 using code")
(ok (equal? 5 (get-code XMM5))
    "Get code of XMM5")
(ok (equal? '(#xb8 #x2a #x00 #x00 #x00) (MOV EAX 42))
    "MOV EAX, 42")
(ok (equal? '(#xb9 #x2a #x00 #x00 #x00) (MOV ECX 42))
    "MOV ECX, 42")
(ok (equal? '(#x41 #xb9 #x2a #x00 #x00 #x00) (MOV R9D 42))
    "MOV R9D, 42")
(ok (equal? '(#x48 #xbe #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00)
            (MOV RSI 42))
    "MOV RSI, 42")
(ok (equal? '(#x49 #xb9 #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00)
            (MOV R9 42))
    "MOV R9, 42")
(ok (equal? '(#xb0 #x2a) (MOV AL 42))
    "MOV AL, 42")
(ok (equal? '(#xb4 #x2a) (MOV AH 42))
    "MOV AH, 42")
(ok (equal? '(#x40 #xb4 #x2a) (MOV SPL 42))
    "MOV SPL, 42")
(ok (equal? '(#x66 #xb8 #x2a #x00) (MOV AX 42))
    "MOV AX, 42")
(ok (equal? '(#x8b #xd8) (MOV EBX EAX))
    "MOV EBX, EAX")
(ok (equal? '(#x8b #xca) (MOV ECX EDX))
    "MOV ECX, EDX")
(ok (equal? '(#x45 #x8b #xc1) (MOV R8D R9D))
    "MOV R8D, R9D")
(ok (equal? '(#x8a #xd8) (MOV BL AL))
    "MOV BL, AL")
(ok (equal? '(#x66 #x8b #xd8) (MOV BX AX))
    "MOV BX, AX")
(ok (equal? '(#x8b #x0a) (MOV ECX (ptr <int> RDX)))
    "MOV ECX, [RDX]")
(ok (equal? "(ptr <int<32,signed>> RDX)" (format #f "~a" (ptr <int> RDX)))
    "Print pointer")
(ok (equal? '(#x8b #x4d #x00) (MOV ECX (ptr <int> RBP)))
    "MOV ECX, [RBP]; special case of REX encoding")
(ok (equal? '(#x48 #x8b #x0a) (MOV RCX (ptr <long> RDX)))
    "MOV RCX, [RDX]")
(ok (equal? '(#x41 #x8b #x0b) (MOV ECX (ptr <int> R11)))
    "MOV ECX, [R11]")
(ok (equal? '(#x41 #x8b #x4d #x00) (MOV ECX (ptr <int> R13)))
    "MOV ECX, [R13]; special case of REX encoding")
(ok (equal? '(#x8b #x4a #x04) (MOV ECX (ptr <int> RDX 4)))
    "MOV ECX, [RDX] + 4")
(ok (equal? "(ptr <int<32,signed>> RDX 4)" (format #f "~a" (ptr <int> RDX 4)))
    "Print pointer with displacement")
(ok (equal? '(#x8b #x8a #x80 #x00 #x00 #x00) (MOV ECX (ptr <int> RDX 128)))
    "MOV ECX, [RDX] + 128")
(ok (equal? '(#x8b #x4c #x24 #x04) (MOV ECX (ptr <int> RSP 4)))
    "MOV ECX, [RSP] + 4")
(ok (equal? '(#x45 #x8b #x53 #x04) (MOV R10D (ptr <int> R11 4)))
    "MOV R10D, [R11] + 4")
(ok (equal? '(#x4d #x8b #x53 #x04) (MOV R10 (ptr <long> R11 4)))
    "MOV R10, [R11] + 4")
(ok (equal? '(#x8a #x0a) (MOV CL (ptr <byte> RDX)))
    "MOV CL, [RDX]")
(ok (equal? '(#x66 #x8b #x0a) (MOV CX (ptr <sint> RDX)))
    "MOV CX, [RDX]")
(ok (equal? '(#x89 #x11) (MOV (ptr <int> RCX) EDX))
    "MOV [RCX], EDX")
(ok (equal? '(#x44 #x89 #x01) (MOV (ptr <int> RCX) R8D))
    "MOV [RCX], R8D")
(ok (equal? '(#x41 #x89 #x02) (MOV (ptr <int> R10) EAX))
    "MOV [R10], EAX")
(ok (equal? '(#x41 #x89 #x04 #x24) (MOV (ptr <int> R12) EAX))
    "MOV [R12], EAX")
(ok (equal? '(#xc3) (RET))
    "RET # near return")
(ok (equal? #vu8(#x8b #xc7 #xc3) (obj (list (MOV EAX EDI) (RET))))
    "'obj' should return bytevector with machine code")
(ok (begin ((asm ctx <null> '() (list (RET)))) #t)
    "Empty function")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV EAX i1) (RET)))))
    "Return constant in EAX")
(ok (eqv? l1 ((asm ctx <long> '() (list (MOV RAX l1) (RET)))))
    "Return constant in RAX")
(ok (eqv? b1 ((asm ctx <byte> '() (list (MOV AL b1) (RET)))))
    "Return constant in AL")
(ok (eqv? w1 ((asm ctx <sint> '() (list (MOV AX w1) (RET)))))
    "Return constant in AX")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV ECX i1) (MOV EAX ECX) (RET)))))
    "Function copying content from ECX")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV R14D i1) (MOV EAX R14D) (RET)))))
    "Function copying content from R14D")
(ok (eqv? (ash 42 32) ((asm ctx <long> '() (list (MOV R14 (ash 42 32)) (MOV RAX R14) (RET)))))
    "Function copying content from R14")
(ok (eqv? b1 ((asm ctx <int> '() (list (MOV DIL b1) (MOV AL DIL) (RET)))))
    "Function copying content from DIL")
(ok (equal? '(#xd1 #xe5) (SHL EBP))
    "SHL EBP, 1")
(ok (equal? '(#x40 #xd0 #xe5) (SHL BPL))
    "SHL BPL, 1")
(ok (equal? '(#x66 #xd1 #xe5) (SHL BP))
    "SHL BP, 1")
(ok (equal? '(#x48 #xd1 #xe5) (SHL RBP))
    "SHL RBP, 1")
(ok (equal? '(#x40 #xd2 #xe5) (SHL BPL CL))
    "SHL BPL, CL")
(ok (equal? '(#x66 #xd3 #xe5) (SHL BP CL))
    "SHL BP, CL")
(ok (equal? '(#xd1 #xe5) (SAL EBP))
    "SAL EBP, 1")
(ok (equal? '(#x48 #xd1 #xe5) (SAL RBP))
    "SAL RBP, 1")
(ok (equal? '(#xd3 #xe5) (SAL EBP CL))
    "SAL EBP, CL")
(ok (equal? '(#x48 #xd3 #xe5) (SAL RBP CL))
    "SAL RBP, CL")
(ok (eqv? (ash i1 1) ((asm ctx <int> '() (list (MOV EAX i1) (SHL EAX) (RET)))))
    "Shift EAX left by 1")
(ok (eqv? (ash i1 1) ((asm ctx <int> '() (list (MOV R9D i1) (SHL R9D) (MOV EAX R9D) (RET)))))
    "Shift R9D left by 1")
(ok (eqv? (ash l1 1) ((asm ctx <long> '() (list (MOV RAX l1) (SHL RAX) (RET)))))
    "Function shifting 64-bit number left by 1")
(ok (eqv? (ash i1 2) ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL 2) (SHL EAX CL) (RET)))))
    "Shift left with generic amount")
(ok (equal? '(#xd1 #xed) (SHR EBP))
    "SHR EBP, 1")
(ok (equal? '(#x48 #xd1 #xed) (SHR RBP))
    "SHR RBP, 1")
(ok (equal? '(#x40 #xd2 #xed) (SHR BPL CL))
    "SHR BL CL")
(ok (equal? '(#xd3 #xed) (SHR EBP CL))
    "SHR EBP, CL")
(ok (equal? '(#xd1 #xfd) (SAR EBP))
    "SAR EBP, 1")
(ok (equal? '(#x48 #xd1 #xfd) (SAR RBP))
    "SAR RBP, 1")
(ok (equal? '(#xd3 #xfd) (SAR EBP CL))
    "SAR EBP, CL")
(ok (equal? '(#x48 #xd3 #xfd) (SAR RBP CL))
    "SAR RBP, CL")
(ok (eqv? (ash i1 -1) ((asm ctx <int> '() (list (MOV EAX i1) (SHR EAX) (RET)))))
    "Function shifting right by 1")
(ok (eqv? -21 ((asm ctx <int> '() (list (MOV EAX -42) (SAR EAX) (RET)))))
    "Function shifting negative number right by 1")
(ok (eqv? (ash l1 -1) ((asm ctx <long> '() (list (MOV RAX l1) (SHR RAX) (RET)))))
    "Function shifting 64-bit number right by 1")
(ok (eqv? (ash i1 -2) ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL 2) (SHR EAX CL) (RET)))))
    "Shift left with generic amount")
(ok (eqv? (ash -1 30) ((asm ctx <long> '()
                            (list (MOV RAX (ash -1 32))
                                  (SAR RAX)
                                  (SAR RAX)
                                  (RET)))))
    "Function shifting signed 64-bit number right by 2")
(ok (equal? '(#x05 #x0d #x00 #x00 #x00) (ADD EAX 13))
    "ADD EAX, 13")
(ok (equal? '(#x48 #x05 #x0d #x00 #x00 #x00) (ADD RAX 13))
    "ADD RAX, 13")
(ok (equal? '(#x66 #x05 #x0d #x00) (ADD AX 13))
    "ADD AX, 13")
(ok (equal? '(#x04 #x0d) (ADD AL 13))
    "ADD AL, 13")
(ok (equal? '(#x81 #xc1 #x0d #x00 #x00 #x00) (ADD ECX 13))
    "ADD ECX, 13")
(ok (equal? '(#x48 #x81 #xc1 #x0d #x00 #x00 #x00) (ADD RCX 13))
    "ADD RCX, 13")
(ok (equal? '(#x66 #x81 #xc1 #x0d #x00) (ADD CX 13))
    "ADD CX, 13")
(ok (equal? '(#x80 #xc1 #x0d) (ADD CL 13))
    "ADD CL, 13")
(ok (equal? '(#x41 #x81 #xc2 #x0d #x00 #x00 #x00) (ADD R10D 13))
    "ADD R10D, 13")
(ok (equal? '(#x49 #x81 #xc2 #x0d #x00 #x00 #x00) (ADD R10 13))
    "ADD R10, 13")
(ok (equal? '(#x80 #x07 #x2a) (ADD (ptr <byte> RDI) 42))
    "ADD BYTE PTR [RDI], 42")
(ok (equal? '(#x03 #xca) (ADD ECX EDX))
    "ADD ECX, EDX")
(ok (equal? '(#x45 #x03 #xf7) (ADD R14D R15D))
    "ADD R14D, R15D")
(ok (equal? '(#x66 #x03 #xca) (ADD CX DX))
    "ADD CX, DX")
(ok (equal? '(#x02 #xca) (ADD CL DL))
    "ADD CL, DL")
(ok (equal? '(#x03 #x0a) (ADD ECX (ptr <int> RDX)))
    "ADD ECX, [RDX]")
(ok (equal? '(#x2d #x0d #x00 #x00 #x00) (SUB EAX 13))
    "SUB EAX, 13")
(ok (equal? '(#x48 #x2d #x0d #x00 #x00 #x00) (SUB RAX 13))
    "SUB RAX, 13")
(ok (equal? '(#x41 #x81 #xea #x0d #x00 #x00 #x00) (SUB R10D 13))
    "SUB R10D, 13")
(ok (equal? '(#x49 #x81 #xea #x0d #x00 #x00 #x00) (SUB R10 13))
    "SUB R10, 13")
(ok (equal? '(#x41 #x81 #x2a #x0d #x00 #x00 #x00) (SUB (ptr <int> R10) 13))
    "SUB (ptr <int> R10), 13")
(ok (equal? '(#x2b #xca) (SUB ECX EDX))
    "SUB ECX, EDX")
(ok (equal? '(#x45 #x2b #xf7) (SUB R14D R15D))
    "SUB R14D, R15D")
(ok (eqv? 55 ((asm ctx <int> '() (list (MOV EAX 42) (ADD EAX 13) (RET)))))
    "Function using EAX to add 42 and 13")
(ok (eqv? 55 ((asm ctx <long> '() (list (MOV RAX 42) (ADD RAX 13) (RET)))))
    "Function using RAX to add 42 and 13")
(ok (eqv? 55 ((asm ctx <sint> '()  (list (MOV AX 42) (ADD AX 13) (RET)))))
    "Function using AX to add 42 and 13")
(ok (eqv? 55 ((asm ctx <byte> '() (list (MOV AL 42) (ADD AL 13) (RET)))))
    "Function using AL to add 42 and 13")
(ok (eqv? 55 ((asm ctx <int> '() (list (MOV R9D 42) (ADD R9D 13) (MOV EAX R9D) (RET)))))
    "Function using R9D to add 42 and 13")
(ok (eqv? 55 ((asm ctx <long> '() (list (MOV R9 42) (ADD R9 13) (MOV RAX R9) (RET)))))
    "Function using R9 to add 42 and 13")
(ok (eqv? 55 ((asm ctx <sint> '() (list (MOV R9W 42) (ADD R9W 13) (MOV AX R9W) (RET)))))
    "Function using R9W to add 42 and 13")
(ok (eqv? 55 ((asm ctx <byte> '() (list (MOV R9L 42) (ADD R9L 13) (MOV AL R9L) (RET)))))
    "Function using R9L to add 42 and 13")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (ADD EAX i2) (RET)))))
    "Function adding two integers in EAX")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '() (list (MOV EDX i1) (ADD EDX i2) (MOV EAX EDX) (RET)))))
    "Function adding two integers in EDX")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '() (list (MOV R10D i1) (ADD R10D i2) (MOV EAX R10D) (RET)))))
    "Function adding two integers in R10D")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (MOV ECX i2) (ADD EAX ECX) (RET)))))
    "Function using EAX and ECX to add two integers")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '()
                          (list (MOV R14D i1)
                                (MOV R15D i2)
                                (ADD R14D R15D)
                                (MOV EAX R14D)
                                (RET)))))
    "Function using R14D and R15D to add two integers")
(ok (eqv? (+ w1 w2) ((asm ctx <sint> '() (list (MOV AX w1) (MOV CX w2) (ADD AX CX) (RET)))))
    "Function using AX and CX to add two short integers")
(ok (eqv? (+ b1 b2) ((asm ctx <byte> '() (list (MOV AL b1) (MOV CL b2) (ADD AL CL) (RET)))))
    "Function using AL and CL to add two bytes")
(ok (eqv? (+ i1 i2) ((asm ctx <int> '()
                          (list (MOV EAX i2)
                                (MOV RDX (idata))
                                (ADD EAX (ptr <int> RDX))
                                (RET)))))
    "Add integer memory operand")
(ok (eqv? (+ i1 i2) (begin ((asm ctx <null> '()
                                 (list (MOV RSI mem)
                                       (MOV (ptr <int> RSI) i1)
                                       (ADD (ptr <int> RSI) i2)
                                       (RET))))
                    (get (fetch iptr))))
    "Add integer to integer in memory")
(ok (equal? '(#x90) (NOP))
    "NOP # no operation")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV EAX i1) (NOP) (NOP) (RET)))))
    "Function with some NOP statements inside")
(ok (equal? '(#x52) (PUSH EDX))
    "PUSH EDX")
(ok (equal? '(#x57) (PUSH EDI))
    "PUSH EDI")
(ok (equal? '(#x5a) (POP EDX))
    "POP EDX")
(ok (equal? '(#x5f) (POP EDI))
    "POP EDI")
(ok (eqv? i1 ((asm ctx <long> '() (list (MOV EDX i1) (PUSH EDX) (POP EAX) (RET)))))
    "Use 32-bit PUSH and POP")
(ok (eqv? l1 ((asm ctx <long> '() (list (MOV RDX l1) (PUSH RDX) (POP RAX) (RET)))))
    "Use 64-bit PUSH and POP")
(ok (eqv? w1 ((asm ctx <long> '() (list (MOV DX w1) (PUSH DX) (POP AX) (RET)))))
    "Use 16-bit PUSH and POP")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV RCX (idata)) (MOV EAX (ptr <int> RCX)) (RET)))))
    "Load integer from address in RCX")
(ok (eqv? i1 ((asm ctx <int> '() (list (MOV R10 (idata)) (MOV EAX (ptr <int> R10)) (RET)))))
    "Load integer from address in R10")
(ok (eqv? i2 ((asm ctx <int> '() (list (MOV RCX (idata)) (MOV EAX (ptr <int> RCX 4)) (RET)))))
    "Load integer from address in RCX with offset")
(ok (eqv? i1 ((asm ctx <int> '()
                   (list (MOV RCX (get (+ iptr 50)))
                         (MOV EAX (ptr <int> RCX -200))
                         (RET)))))
    "Load integer from address in RCX with large offset")
(ok (eqv? i2 ((asm ctx <int> '() (list (MOV R9 (idata)) (MOV EAX (ptr <int> R9 4)) (RET)))))
    "Load integer from address in R9D with offset")
(ok (eqv? l1 ((asm ctx <long> '() (list (MOV RCX (ldata)) (MOV RAX (ptr <long> RCX)) (RET)))))
    "Load long integer from address in RCX")
(ok (eqv? l2 ((asm ctx <long> '() (list (MOV RCX (ldata)) (MOV RAX (ptr <long> RCX 8)) (RET)))))
    "Load long integer from address in RCX with offset")
(ok (eqv? w1 ((asm ctx <sint> '() (list (MOV RCX (wdata)) (MOV AX (ptr <sint> RCX)) (RET)))))
    "Load short integer from address in RCX")
(ok (eqv? w2 ((asm ctx <sint> '() (list (MOV RCX (wdata)) (MOV AX (ptr <sint> RCX 2)) (RET)))))
    "Load short integer from address in RCX with offset")
(ok (eqv? b1 ((asm ctx <byte> '() (list (MOV RCX (bdata)) (MOV AL (ptr <byte> RCX)) (RET)))))
    "Load byte from address in RCX")
(ok (eqv? b2 ((asm ctx <byte> '() (list (MOV RCX (bdata)) (MOV AL (ptr <byte> RCX 1)) (RET)))))
    "Load byte from address in RCX with offset")
(ok (equal? '(#x48 #x8d #x51 #x04) (LEA RDX (ptr <int> RCX 4)))
    "LEA RDX, [RCX + 4]")
(ok (equal? '(#x48 #x8d #x44 #x24 #xfc) (LEA RAX (ptr <int> RSP -4)))
    "LEA RAX, [RSP - 4]")
(ok (eqv? i2 ((asm ctx <int> '()
                   (list (MOV RCX (idata))
                         (LEA RDX (ptr <int> RCX 4))
                         (MOV EAX (ptr <int> RDX))
                         (RET)))))
    "Load integer from address in RCX with offset using effective address")
(ok (equal? '(#x48 #x8d #x04 #xb7) (LEA RAX (ptr <int> RDI RSI)))
    "LEA RAX, [RDI + ESI * 4]")
(ok (equal? "(ptr <int<32,signed>> RDI RSI)" (format #f "~a" (ptr <int> RDI RSI)))
    "Print pointer with index")
(ok (eqv? i2 ((asm ctx <int> '()
                   (list (MOV RCX (idata))
                         (MOV RDI 1)
                         (LEA RDX (ptr <int> RCX RDI))
                         (MOV EAX (ptr <int> RDX))
                         (RET)))))
    "Load integer from address in RCX with index times 4 using effective address")
(ok (equal? '(#x48 #x8d #x04 #x77) (LEA RAX (ptr <sint> RDI RSI)))
    "LEA RAX, [RDI + ESI * 2]")
(ok (eqv? i2 ((asm ctx <int> '()
                   (list (MOV RCX (idata))
                         (MOV RDI 2)
                         (LEA RDX (ptr <sint> RCX RDI))
                         (MOV EAX (ptr <int> RDX))
                         (RET)))))
    "Load integer from address in RCX with index times 2 using effective address")
(ok (equal? '(#x48 #x8d #x44 #x77 #x02) (LEA RAX (ptr <sint> RDI RSI 2)))
    "LEA RAX, [RDI + ESI * 2 + 2]")
(ok (equal? "(ptr <int<16,signed>> RDI RSI 2)" (format #f "~a" (ptr <sint> RDI RSI 2)))
    "Print pointer with index and offset")
(ok (eqv? i2 ((asm ctx <int> '()
                   (list (MOV RCX (idata))
                         (MOV RDI 3)
                         (LEA RDX (ptr <byte> RCX RDI 1))
                         (MOV EAX (ptr <int> RDX))
                         (RET)))))
    "Load integer from address in RCX with index and offset using effective address")
(ok (eqv? i2 ((asm ctx <int> '()
                   (list (MOV R9 (idata))
                         (MOV R10 3)
                         (LEA R11 (ptr <byte> R9 R10 1))
                         (MOV EAX (ptr <int> R11))
                         (RET)))))
    "Load integer from address in R9 with index and offset using effective address")
(ok (eqv? #x08 (begin ((asm ctx <long> '()
                            (list (MOV RDI (idx))
                                  (MOV AL (ptr <byte> RDI))
                                  (RET))))))
    "Load 8-bit value from memory")
(ok (eqv? #x0708 (begin ((asm ctx <long> '()
                              (list (MOV RDI (idx))
                                    (MOV AX (ptr <sint> RDI))
                                    (RET))))))
    "Load 16-bit value from memory")
(ok (eqv? #x05060708 (begin ((asm ctx <long> '()
                                  (list (MOV RDI (idx))
                                        (MOV EAX (ptr <int> RDI))
                                        (RET))))))
    "Load 32-bit value from memory")
(ok (eqv? #x0102030405060708 (begin ((asm ctx <long> '()
                                          (list (MOV RDI (idx))
                                                (MOV RAX (ptr <long> RDI))
                                                (RET))))))
    "Load 64-bit value from memory")
(ok (eqv? i1 (begin ((asm ctx <null> '()
                          (list (MOV RSI mem)
                                (MOV ECX i1)
                                (MOV (ptr <int> RSI) ECX)
                                (RET))))
                    (get (fetch iptr))))
    "Write value of ECX to memory")
(ok (eqv? l1 (begin ((asm ctx <null> '()
                          (list (MOV RSI mem)
                                (MOV RCX l1)
                                (MOV (ptr <long> RSI) RCX)
                                (RET))))
                    (get (fetch lptr))))
    "Write value of RCX to memory")
(ok (eqv? i1 (begin ((asm ctx <int> '()
                          (list (MOV RSI mem)
                                (MOV R8D i1)
                                (MOV (ptr <int> RSI) R8D)
                                (RET))))
                    (get (fetch iptr))))
    "Write value of R8D to memory")
(ok (eqv? i1 (begin ((asm ctx <null> '()
                          (list (MOV RSI mem)
                                (MOV ECX i1)
                                (MOV (ptr <int> RSI) ECX)
                                (RET))))
                    (get (fetch iptr))))
    "Write value of ECX to memory")
(ok (equal? '(#xc7 #x07 #x2a #x00 #x00 #x00) (MOV (ptr <int> RDI) 42))
    "MOV DWORD PTR [RDI], 42")
(ok (equal? '(#x48 #xc7 #x07 #x2a #x00 #x00 #x00) (MOV (ptr <long> RDI) 42))
    "MOV QWORD PTR [RDI], 42")
(ok (equal? '(#x66 #xc7 #x07 #x2a #x00) (MOV (ptr <sint> RDI) 42))
    "MOV WORD PTR [RDI], 42")
(ok (equal? '(#xc6 #x07 #x2a) (MOV (ptr <byte> RDI) 42))
    "MOV BYTE PTR [RDI], 42")
(ok (eqv? #x0102030405060700 (begin ((asm ctx <null> '()
                                          (list (MOV RDI (idx))
                                                (MOV (ptr <byte> RDI) 0)
                                                (RET))))
                                    (get (fetch lptr))))
    "Write 8-bit value to memory")
(ok (eqv? #x0102030405060000 (begin ((asm ctx <null> '()
                                          (list (MOV R10 (idx))
                                                (MOV (ptr <sint> R10) 0)
                                                (RET))))
                                    (get (fetch lptr))))
    "Write 16-bit value to memory")
(ok (eqv? #x0102030400000000 (begin ((asm ctx <null> '()
                                          (list (MOV RDI (idx))
                                                (MOV (ptr <int> RDI) 0)
                                                (RET))))
                                    (get (fetch lptr))))
    "Write 32-bit value to memory")
(ok (eqv? #x0000000000000000 (begin ((asm ctx <null> '()
                                          (list (MOV RDI (idx))
                                                (MOV (ptr <long> RDI) 0)
                                                (RET))))
                                    (get (fetch lptr))))
    "Write 64-bit value to memory")
(ok (eqv? 2 ((asm ctx <int> (make-list 4 <int>)
                  (list (MOV EAX EDI) (RET))) 2 3 5 7))
    "Return first integer argument")
(ok (eqv? 13 ((asm ctx <int> (make-list 6 <int>)
                   (list (MOV EAX R9D) (RET))) 2 3 5 7 11 13))
    "Return sixth integer argument")
(ok (eqv? 17 ((asm ctx <int> (make-list 8 <int>)
                   (list (MOV EAX (ptr <int> RSP #x8)) (RET))) 2 3 5 7 11 13 17 19))
    "Return seventh integer argument")
(ok (eqv? 19 ((asm ctx <int> (make-list 8 <int>)
                   (list (MOV EAX (ptr <int> RSP #x10)) (RET))) 2 3 5 7 11 13 17 19))
    "Return eighth integer argument")
(ok (equal? '(#xf7 #xd3) (NOT EBX))
    "NOT EBX")
(ok (equal? '(#x66 #xf7 #xd3) (NOT BX))
    "NOT BX")
(ok (equal? '(#xf6 #xd3) (NOT BL))
    "NOT BL")
(ok (equal? '(#xf7 #xdb) (NEG EBX))
    "NEG EBX")
(ok (equal? '(#x66 #xf7 #xdb) (NEG BX))
    "NEG BX")
(ok (equal? '(#xf6 #xdb) (NEG BL))
    "NEG BL")
(ok (equal? '(#xff #xc3) (INC EBX))
    "INC EBX")
(ok (equal? '(#x66 #xff #xc3) (INC BX))
    "INC BX")
(ok (equal? '(#xfe #xc3) (INC BL))
    "INC BL")
(ok (eqv? (- i1) ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (NEG EAX) (RET))) i1))
    "Function negating an integer")
(ok (eqv? (- l1) ((asm ctx <long> (list <long>) (list (MOV RAX RDI) (NEG RAX) (RET))) l1))
    "Function negating a long integer")
(ok (eqv? (- w1) ((asm ctx <sint> (list <sint>) (list (MOV AX DI) (NEG AX) (RET))) w1))
    "Function negating a short integer")
(ok (eqv? (- b1) ((asm ctx <byte> (list <byte>) (list (MOV AL DIL) (NEG AL) (RET))) b1))
    "Function negating a byte")
(ok (eqv? (- i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (SUB EAX i2) (RET)))))
    "Function subtracting two integers in EAX")
(ok (eqv? (- i1 i2) ((asm ctx <int> '() (list (MOV EDX i1) (SUB EDX i2) (MOV EAX EDX) (RET)))))
    "Function subtracting two integers in EDX")
(ok (eqv? (- i1 i2) ((asm ctx <int> '() (list (MOV R10D i1) (SUB R10D i2) (MOV EAX R10D) (RET)))))
    "Function subtracting two integers in R10D")
(ok (eqv? (- i1 i2) ((asm ctx <int> '() (list (MOV EAX i1) (MOV ECX i2) (SUB EAX ECX) (RET)))))
    "Function using EAX and ECX to subtract two integers")
(ok (eqv? (- i1 i2) ((asm ctx <int> '()
                          (list (MOV R14D i1)
                                (MOV R15D i2)
                                (SUB R14D R15D)
                                (MOV EAX R14D)
                                (RET)))))
    "Function using R14D and R15D to subtract two integers")
(ok (equal? '(#x48 #x0f #xaf #xca) (IMUL RCX RDX))
    "IMUL RCX, RDX")
(ok (eqv? (* w1 w2) ((asm ctx <int> '() (list (MOV EAX w1) (MOV ECX w2) (IMUL EAX ECX) (RET)))))
    "Function using EAX and ECX to multiply two short integers")
(ok (eqv? (* i1 i2) ((asm ctx <long> '() (list (MOV RAX i1) (MOV RCX i2) (IMUL RAX RCX) (RET)))))
    "Function using RAX and RCX to multiply two integers")
(ok (equal? '(#x41 #x6b #xc2 #x04) (IMUL EAX R10D 4))
    "IMUL EAX, R10D, 4")
(ok (equal? '(#x41 #x69 #xc2 #x00 #x04 #x00 #x00) (IMUL EAX R10D 1024))
    "IMUL EAX, R10D, 1024")
(ok (equal? '(#x41 #x69 #xc2 #x7f #xff #xff #xff) (IMUL EAX R10D -129))
    "IMUL EAX, R10D, -129")
(ok (equal? '(#x66 #x41 #x69 #xc2 #x00 #x04) (IMUL AX R10W 1024))
    "IMUL AX, R10W, 1024")
(ok (eqv? (* w1 b2) ((asm ctx <int> (list <int>) (list (IMUL EAX EDI b2) (RET))) w1))
    "Function multiplying an integer with a byte constant")
(ok (equal? '(#xf7 #xfe) (IDIV ESI))
    "IDIV ESI")
(ok (equal? '(#x40 #xf6 #xfe) (IDIV SIL))
    "IDIV SIL")
(ok (equal? '(#x66 #xf7 #xfe) (IDIV SI))
    "IDIV SI")
(ok (equal? '(#xf7 #xf6) (DIV ESI))
    "DIV ESI")
(ok (equal? '(#x40 #xf6 #xf6) (DIV SIL))
    "DIV SIL")
(ok (equal? '(#x66 #xf7 #xf6) (DIV SI))
    "DIV SI")
(ok (equal? '(#x25 #x0d #x00 #x00 #x00) (AND EAX 13))
    "AND EAX, 13")
(ok (equal? '(#x81 #xe1 #x0d #x00 #x00 #x00) (AND ECX 13))
    "AND ECX, 13")
(ok (equal? '(#x80 #x27 #x2a) (AND (ptr <byte> RDI) 42))
    "AND BYTE PTR [RDI], 42")
(ok (equal? '(#x23 #xca) (AND ECX EDX))
    "AND ECX, EDX")
(ok (equal? '(#x23 #x0a) (AND ECX (ptr <int> RDX)))
    "AND ECX, [RDX]")
(ok (equal? '(#x0d #x0d #x00 #x00 #x00) (OR EAX 13))
    "OR EAX, 13")
(ok (equal? '(#x81 #xc9 #x0d #x00 #x00 #x00) (OR ECX 13))
    "OR ECX, 13")
(ok (equal? '(#x80 #x0f #x2a) (OR (ptr <byte> RDI) 42))
    "OR BYTE PTR [RDI], 42")
(ok (equal? '(#x0b #xca) (OR ECX EDX))
    "OR ECX, EDX")
(ok (equal? '(#x0b #x0a) (OR ECX (ptr <int> RDX)))
    "OR ECX, [RDX]")
(ok (equal? '(#x35 #x0d #x00 #x00 #x00) (XOR EAX 13))
    "XOR EAX, 13")
(ok (equal? '(#x81 #xf1 #x0d #x00 #x00 #x00) (XOR ECX 13))
    "XOR ECX, 13")
(ok (equal? '(#x80 #x37 #x2a) (XOR (ptr <byte> RDI) 42))
    "XOR BYTE PTR [RDI], 42")
(ok (equal? '(#x33 #xca) (XOR ECX EDX))
    "XOR ECX, EDX")
(ok (equal? '(#x33 #x0a) (XOR ECX (ptr <int> RDX)))
    "XOR ECX, [RDX]")
(ok (not (throws? (asm ctx <int> '() (list (MOV EAX i1) 'tst (RET)))))
    "Assembler should tolerate labels")
(ok (equal? '(#xeb #x2a) (JMP 42))
    "JMP 42")
(ok (equal? '(#xe9 #xa4 #x01 #x00 #x00) (JMP 420))
    "JMP 420")
(ok (equal? (list (NOP) (NOP)) (resolve-jumps (list (NOP) 'tst (NOP))))
    "Remove label information from program")
(ok (equal? (list (JMP 0)) (resolve-jumps (list (JMP 'tst) 'tst)))
    "Resolve jump address in trivial program")
(ok (equal? (list (JMP -2)) (resolve-jumps (list 'tst (JMP 'tst))))
    "Resolve backward jump")
(ok (not (conditional? (JMP 'tst)))
    "JMP is not a conditional jump")
(ok (equal? (list (JMP 1) (NOP)) (resolve-jumps (list (JMP 'tst) (NOP) 'tst)))
    "Resolve jump address in program with trailing NOP")
(ok (equal? (list (NOP) (JMP 0)) (resolve-jumps (list (NOP) (JMP 'tst) 'tst)))
    "Resolve jump address in program with leading NOP")
(ok (equal? (list (JMP 2) (JMP 0)) (resolve-jumps (list (JMP 'tst) (JMP 'tst) 'tst)))
    "Resolve multiple jump statements")
(ok (equal? (list (JMP 2) (JMP -4)) (resolve-jumps (list 'a (JMP 'b) (JMP 'a) 'b)))
    "Resolve multiple jump addresses")
(ok (equal? (list #xeb #x7f) (car (resolve-jumps (append (list (JMP 'a)) (make-list 127 (NOP)) (list 'a)))))
    "Resolve a jump of 127 bytes")
(ok (equal? (list #xe9 #x80 #x00 #x00 #x00)
            (car (resolve-jumps (append (list (JMP 'a)) (make-list 128 (NOP)) (list 'a)))))
    "Resolve a jump of 128 bytes")
(ok (equal? (list #xeb #x05)
            (car (resolve-jumps (append (list (JMP 'a) (JMP 'b) 'a) (make-list 128 (NOP)) (list 'b)))))
    "Resolve a jump over a long jump statement")
(ok (eqv? i1 ((asm ctx <int> '()
                   (list (MOV ECX i1)
                         (JMP 'tst)
                         (MOV ECX 0)
                         'tst
                         (MOV EAX ECX)
                         (RET)))))
    "Function with a local jump")
(ok (eqv? i1 ((asm ctx <int> '()
                   (list (MOV EAX 0)
                         (JMP 'b)
                         'a
                         (MOV EAX i1)
                         (JMP 'c)
                         'b
                         (MOV EAX i2)
                         (JMP 'a)
                         'c
                         (RET)))))
    "Function with several local jumps")
(ok (equal? '(#x3d #x2a #x00 #x00 #x00) (CMP EAX 42))
    "CMP EAX 42")
(ok (equal? '(#x48 #x3d #x2a #x00 #x00 #x00) (CMP RAX 42))
    "CMP RAX 42")
(ok (equal? '(#x41 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10D 42))
    "CMP R10D 42")
(ok (equal? '(#x49 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10 42))
    "CMP R10 42")
(ok (equal? '(#x41 #x0f #x94 #xc1) (SETE R9L))
    "SETE R9L")
(ok (equal? '(#x0f #x92 #xc2) (SETB DL))
    "SETB DL")
(ok (eqv? 1 ((asm ctx <byte> (list <int>) (list (MOV EAX EDI) (CMP EAX 0) (SETE AL) (RET))) 0))
    "Compare zero in EAX with zero")
(ok (eqv? 0 ((asm ctx <byte> (list <int>) (list (MOV EAX EDI) (CMP EAX 0) (SETE AL) (RET))) (logior 1 i1)))
    "Compare non-zero number in EAX with zero")
(ok (equal? '(#x41 #x81 #xfa #x2a #x00 #x00 #x00) (CMP R10D 42))
    "CMP R10D 42")
(ok (eqv? 1 ((asm ctx <byte> (list <int>) (list (MOV R10D EDI) (CMP R10D 0) (SETE AL) (RET))) 0))
    "Compare zero in R10D with zero")
(ok (eqv? 0 ((asm ctx <byte> (list <int>) (list (MOV R10D EDI) (CMP R10D 0) (SETE AL) (RET))) (logior 1 i1)))
    "Compare non-zero number in R10D with zero")
(ok (equal? '(#x3b #xfe) (CMP EDI ESI))
    "CMP EDI ESI")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETE AL) (RET))) i1 i1))
    "Two integers being equal")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETE AL) (RET))) i1 (logxor 1 i1)))
    "Two integers not being equal")
(ok (equal? '(#x48 #x3b #xf7) (CMP RSI RDI))
    "CMP RSI RDI")
(ok (eqv? 1 ((asm ctx <byte> (list <long> <long>) (list (CMP RSI RDI) (SETE AL) (RET))) l1 l1))
    "Two long integers being equal")
(ok (eqv? 0 ((asm ctx <byte> (list <long> <long>) (list (CMP RSI RDI) (SETE AL) (RET))) l1 (logxor 1 l1)))
    "Two long integers not being equal")
(ok (equal? '(#x41 #x0f #x92 #xc1) (SETB R9L))
    "SETB R9L")
(ok (eqv? 1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETB AL) (RET))) 1 3))
    "Unsigned integer being below another")
(ok (eqv? 0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETB AL) (RET))) 3 3))
    "Unsigned integer not being below another")
(ok (equal? '(#x41 #x0f #x93 #xc1) (SETNB R9L))
    "SETNB R9L")
(ok (eqv? 0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNB AL) (RET))) 1 3))
    "Unsigned integer not being above or equal")
(ok (eqv? 1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNB AL) (RET))) 3 3))
    "Unsigned integer being above or equal")
(ok (equal? '(#x41 #x0f #x95 #xc1) (SETNE R9L))
    "SETNE R9L")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNE AL) (RET))) i1 i1))
    "Two integers not being unequal")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNE AL) (RET))) i1 (logxor 1 i1)))
    "Two integers being unequal")
(ok (equal? '(#x41 #x0f #x96 #xc1) (SETBE R9L))
    "SETBE R9L")
(ok (eqv? 1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETBE AL) (RET))) 3 3))
    "Unsigned integer being below or equal")
(ok (eqv? 0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETBE AL) (RET))) 4 3))
    "Unsigned integer not being below or equal")
(ok (equal? '(#x41 #x0f #x97 #xc1) (SETNBE R9L))
    "SETNBE R9L")
(ok (eqv? 0 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNBE AL) (RET))) 3 3))
    "Unsigned integer not being above")
(ok (eqv? 1 ((asm ctx <byte> (list <uint> <uint>) (list (CMP EDI ESI) (SETNBE AL) (RET))) 4 3))
    "Unsigned integer being above")
(ok (equal? '(#x41 #x0f #x9c #xc1) (SETL R9L))
    "SETL R9L")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETL AL) (RET))) -2 3))
    "Signed integer being less")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETL AL) (RET))) 3 3))
    "Signed integer not being less")
(ok (equal? '(#x41 #x0f #x9d #xc1) (SETNL R9L))
    "SETNL R9L")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNL AL) (RET))) -2 3))
    "Signed integer not being greater or equal")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNL AL) (RET))) 3 3))
    "Signed integer being greater or equal")
(ok (equal? '(#x41 #x0f #x9e #xc1) (SETLE R9L))
    "SETLE R9L")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETLE AL) (RET))) -2 -2))
    "Signed integer being less or equal")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETLE AL) (RET))) 3 -2))
    "Signed integer not being less or equal")
(ok (equal? '(#x41 #x0f #x9f #xc1) (SETNLE R9L))
    "SETNLE R9L")
(ok (eqv? 0 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNLE AL) (RET))) -2 -2))
    "Signed integer not being greater")
(ok (eqv? 1 ((asm ctx <byte> (list <int> <int>) (list (CMP EDI ESI) (SETNLE AL) (RET))) 3 -2))
    "Signed integer being greater")
(ok (equal? '(#x74 #x2a) (JE 42))
    "JE 42")
(ok (conditional? (JE 'tst))
    "JE is a conditional jump")
(ok (eqv? 1 ((asm ctx <int> '() (list (MOV EAX 1) (CMP EAX 1) (JE 'l) (MOV EAX 0) 'l (RET)))))
    "Test JE with ZF=1")
(ok (eqv? 0 ((asm ctx <int> '() (list (MOV EAX 2) (CMP EAX 1) (JE 'l) (MOV EAX 0) 'l (RET)))))
    "Test JE with ZF=0")
(ok (equal? '(#x72 #x2a) (JB 42))
    "JB 42")
(ok (eqv? 3 ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (CMP EAX 5) (JB 'l) (MOV EAX 5) 'l (RET))) 3))
    "Test JB with CF=1")
(ok (eqv? 5 ((asm ctx <int> (list <int>) (list (MOV EAX EDI) (CMP EAX 5) (JB 'l) (MOV EAX 5) 'l (RET))) 7))
    "Test JB with CF=0")
(ok (equal? '(#x75 #x2a) (JNE 42))
    "JNE 42")
(ok (equal? '(#x76 #x2a) (JBE 42))
    "JBE 42")
(ok (equal? '(#x77 #x2a) (JNBE 42))
    "JNBE 42")
(ok (equal? '(#x7c #x2a) (JL 42))
    "JL 42")
(ok (equal? '(#x7d #x2a) (JNL 42))
    "JNL 42")
(ok (equal? '(#x7e #x2a) (JLE 42))
    "JLE 42")
(ok (equal? '(#x7f #x2a) (JNLE 42))
    "JNLE 42")
(ok (equal? '(#xa9 #x2a #x00 #x00 #x00) (TEST EAX 42))
    "TEST EAX 42")
(ok (equal? '(#xf7 #xc1 #x2a #x00 #x00 #x00) (TEST ECX 42))
    "TEST ECX 42")
(ok (equal? '(#x85 #xff) (TEST EDI EDI))
    "TEST EDI EDI")
(ok (equal? '(#x66 #x0f #xbe #xca) (MOVSX CX DL))
    "MOVSX CX DL")
(ok (eqv? b1 ((asm ctx <sint> '() (list (MOV AX w1) (MOV CL b1) (MOVSX AX CL) (RET)))))
    "Convert byte to short integer")
(ok (eqv? -42 ((asm ctx <sint> '() (list (MOV AX w1) (MOV CL -42) (MOVSX AX CL) (RET)))))
    "Convert negative byte to short integer")
(ok (equal? '(#x0f #xbe #xca) (MOVSX ECX DL))
    "MOVSX ECX DL")
(ok (eqv? b1 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CL b1) (MOVSX EAX CL) (RET)))))
    "Convert byte to integer")
(ok (eqv? -42 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CL -42) (MOVSX EAX CL) (RET)))))
    "Convert negative byte to integer")
(ok (equal? '(#x48 #x0f #xbe #xca) (MOVSX RCX DL))
    "MOVSX RCX DL")
(ok (eqv? b1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CL b1) (MOVSX RAX CL) (RET)))))
    "Convert byte to short integer")
(ok (eqv? -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CL -42) (MOVSX RAX CL) (RET)))))
    "Convert negative byte to short integer")
(ok (equal? '(#x0f #xbf #xca) (MOVSX ECX DX))
    "MOVSX ECX DX")
(ok (eqv? w1 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CX w1) (MOVSX EAX CX) (RET)))))
    "Convert short integer to integer")
(ok (eqv? -42 ((asm ctx <int> '() (list (MOV EAX i1) (MOV CX -42) (MOVSX EAX CX) (RET)))))
    "Convert negative short integer to integer")
(ok (equal? '(#x48 #x0f #xbf #xca) (MOVSX RCX DX))
    "MOVSX RCX DX")
(ok (eqv? w1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CX w1) (MOVSX RAX CX) (RET)))))
    "Convert short integer to long integer")
(ok (eqv? -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV CX -42) (MOVSX RAX CX) (RET)))))
    "Convert negative short integer to long integer")
(ok (equal? '(#x48 #x63 #xca) (MOVSX RCX EDX))
    "MOVSX RCX EDX")
(ok (eqv? i1 ((asm ctx <long> '() (list (MOV RAX l1) (MOV ECX i1) (MOVSX RAX ECX) (RET)))))
    "Convert integer to long integer")
(ok (eqv? -42 ((asm ctx <long> '() (list (MOV RAX l1) (MOV ECX -42) (MOVSX RAX ECX) (RET)))))
    "Convert negative integer to long integer")
(ok (equal? '(#x66 #x0f #xb6 #xc8) (MOVZX CX AL))
    "MOVZX CX AL")
(ok (eqv? b1 ((asm ctx <usint> '() (list (MOV AX w1) (MOV CL b1) (MOVZX AX CL) (RET)))))
    "Convert unsigned byte to unsigned short integer")
(ok (equal? '(#x0f #xb6 #xc8) (MOVZX ECX AL))
    "MOVZX ECX AL")
(ok (eqv? b1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CL b1) (MOVZX EAX CL) (RET)))))
    "Convert unsigned byte to unsigned integer")
(ok (equal? '(#x0f #xb7 #xc8) (MOVZX ECX AX))
    "MOVZX ECX AX")
(ok (eqv? w1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CX w1) (MOVZX EAX CX) (RET)))))
    "Convert unsigned short integer to unsigned integer")
(ok (equal? '(#x0f #xb7 #xc8) (MOVZX ECX AX))
    "MOVZX ECX AX")
(ok (eqv? w1 ((asm ctx <uint> '() (list (MOV EAX i1) (MOV CX w1) (MOVZX EAX CX) (RET)))))
    "Convert unsigned short integer to unsigned integer")
(ok (equal? '(#x0f #xb7 #xc8) (MOVZX ECX AX))
    "MOVZX ECX AX")
(ok  (throws? (MOVZX RCX EAX))
    "MOVZX RCX EAX should throw error")
(ok (eqv? w1 ((asm ctx <ulong> '() (list (MOV RAX l1) (MOV CX w1) (MOVZX RAX CX) (RET)))))
    "Convert unsigned short integer to unsigned long integer")
(ok (eqv? i1 ((asm ctx <ulong> '() (list (MOV RAX l1) (MOV ECX i1) (MOV RAX ECX) (RET)))))
    "Convert unsigned integer to unsigned long integer")
(ok (eqv? 42 ((asm ctx <int> '() (list (PUSH RBX) (MOV EBX 42) (MOV EAX EBX) (POP RBX) (RET)))))
    "Save and restore value of RBX using the stack (this will crash if it does not restore RBX properly)")
(ok (equal? '(#x66 #x98) (CBW))
    "CBW")
(ok (equal? '(#x98) (CWDE))
    "CWDE")
(ok (equal? '(#x48 #x98) (CDQE))
    "CDQE")
(ok (eqv? (- b1) ((asm ctx <sint> '() (list (MOV AX w1) (MOV AL (- b1)) (CBW) (RET)))))
    "Sign-extend AL")
(ok (eqv? (- w1) ((asm ctx <int> '() (list (MOV EAX i1) (MOV AX (- w1)) (CWDE) (RET)))))
    "Sign-extend AX")
(ok (eqv? (- i1) ((asm ctx <long> '() (list (MOV RAX l1) (MOV EAX (- i1)) (CDQE) (RET)))))
    "Sign-extend EAX")
(ok (equal? '(#x66 #x99) (CWD))
    "CWD")
(ok (equal? '(#x99) (CDQ))
    "CDQ")
(ok (equal? '(#x48 #x99) (CQO))
    "CQO")
(ok (eqv? #xffff ((asm ctx <usint> '() (list (MOV AX -42) (CWD) (MOV AX DX) (RET)))))
    "Sign extend AX into DX:AX")
(ok (eqv? #xffffffff ((asm ctx <uint> '() (list (MOV EAX -42) (CDQ) (MOV EAX EDX) (RET)))))
    "Sign extend EAX into EDX:EAX")
(ok (eqv? #xffffffffffffffff ((asm ctx <ulong> '() (list (MOV RAX -42) (CQO) (MOV RAX RDX) (RET)))))
    "Sign extend RAX into RDX:RAX")
(ok (eqv? 42 ((asm ctx <int> '()
                   (list (MOV (ptr <long> RSP -8) RBX)
                         (SUB RSP 8)
                         (MOV EBX 42)
                         (MOV EAX EBX)
                         (MOV RBX (ptr <long> RSP))
                         (ADD RSP 8)
                         (RET)))))
    "Explicitely manage stack pointer (this will crash if it does not restore RBX and RSP properly)")
(ok (equal? 11 ((asm ctx
                     <int>
                     (make-list 11 <int>)
                     (list (MOV EAX (ptr <int> RSP #x28)) (RET)))
                1 2 3 4 5 6 7 8 9 10 11))
    "Check whether this Guile version supports foreign calls with more than 10 arguments")
(ok (equal? '(#x0f #x42 #xce) (CMOVB ECX ESI))
    "CMOVB ECX ESI")
(ok (equal? '(#x0f #x43 #xce) (CMOVNB ECX ESI))
    "CMOVNB ECX ESI")
(run-tests)
