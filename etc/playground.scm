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

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (predefined '()))
  "Linear scan register allocation for a given program"
  (let* [(live         (live-analysis prog '())); TODO: specify return values here
         (all-vars     (variables prog))
         (intervals    (live-intervals live all-vars))
         (allocation   (linear-scan-coloring intervals registers predefined)); TODO: allocate temporary for each statement
         (locations    (add-spill-information allocation 8 8))]
    (adjust-stack-pointer 8 (concatenate (map (cut replace-variables <> locations RAX) prog))))); TODO: adjust stack pointer

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (x (var <sint>))]
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
              (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                    #:registers (list RSI)))
      "'linear-scan-allocate' should spill variables"))

(run-tests)
