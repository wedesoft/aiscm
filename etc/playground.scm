(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (system foreign)
             (aiscm element)
             (aiscm int)
             (aiscm sequence)
             (aiscm mem)
             (aiscm pointer)
             (aiscm rgb)
             (aiscm obj)
             (aiscm asm)
             (aiscm jit)
             (aiscm method)
             (aiscm util)
             (guile-tap))

(define default-registers (list RAX RCX RDX RSI RDI R10 R11 R9 R8 RBX R12 R13 R14 R15))

; TODO: use RAX for temporary retrieval of spilled variables (or RDX?)
; TODO: force "output" of each command into (temporary) register
; TODO: blocked registers

(run-tests)

(define v (var <int>))
(define prog (list (MOV v 42) (RET)))
(define intervals (live-intervals (live-analysis prog '()) (variables prog)))
(define allocation (linear-scan intervals default-registers))
