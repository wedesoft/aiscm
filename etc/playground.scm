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

(define (linear-allocate live-intervals register-use variable-use . result)
  "Recursively perform one-pass register allocation"
  (if (null? live-intervals)
      result
      (let* [(candidate    (car live-intervals))
             (variable     (car candidate))
             (interval     (cdr candidate))
             (first-index  (car interval))
             (last-index   (cdr interval))
             (variable-use (mark-used-till variable-use variable last-index))
             (register     (find-available register-use first-index))
             (recursion    (lambda (result register)
                             (apply linear-allocate
                                     (cdr live-intervals)
                                     (mark-used-till register-use register last-index)
                                     variable-use
                                     (assq-set result variable register))))]
        (if register
          (recursion result register)
          (let* [(spill-candidate (longest-use variable-use))
                 (register        (assq-ref result spill-candidate))]
            (recursion (assq-set result spill-candidate #f) register))))))

(define (linear-scan-coloring live-intervals registers)
  "Linear scan register allocation based on live intervals"
  (linear-allocate (sort-by live-intervals cadr)
                   (initial-register-use registers)
                   '()))

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (predefined '()))
  "Linear scan register allocation for a given program"
  (let* [(live         (live-analysis prog '())); TODO: specify return values here
         (all-vars     (variables prog))
         (intervals    (live-intervals live all-vars))
         (substitution (linear-scan-coloring intervals registers))]
    (adjust-stack-pointer 8 (substitute-variables prog substitution))))

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (ok (equal? (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 42) (RET))))
      "Allocate a single register")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
      "Allocate a single register using custom list of registers")
  (ok (equal? (list (SUB RSP 8) (MOV EAX 1) (MOV ECX 2) (ADD EAX ECX) (MOV ECX EAX) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
      "Allocate multiple registers")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 1) (ADD ECX ESI) (MOV EAX ECX) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV b 1) (ADD b a) (MOV c b) (RET))
                                 #:predefined (list (cons a RSI) (cons c RAX))))
      "Register allocation with predefined registers"))

(run-tests)

(define v (var <int>))
(define prog (list (MOV v 42) (RET)))
(define intervals (live-intervals (live-analysis prog '()) (variables prog)))
(define allocation (linear-scan-coloring intervals default-registers))
