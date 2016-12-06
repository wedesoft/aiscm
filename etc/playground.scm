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

(define (predefined-variable-use live-intervals predefined)
  "Check until what point predefined variables are used"
  (map (lambda (variable) (cons variable (1+ (cdr (assq-ref live-intervals variable))))) (map car predefined)))

(define (register-predefined-till predefined-use predefined register)
  "Check whether a register is used by a predefined variable and until what point in the program"
  (let [(variable (assq-ref (alist-invert predefined) register))]
    (if variable (assq-ref predefined-use variable) 0)))

(define (initial-register-use registers predefined-use predefined)
  "Initially all registers are available from index zero on"
  (map (lambda (register) (cons register (register-predefined-till predefined-use predefined register))) registers))

(define (linear-scan-coloring live-intervals registers predefined)
  "Linear scan register allocation based on live intervals"
  (define (linear-allocate live-intervals register-use variable-use result)
    (if (null? live-intervals)
        result
        (let* [(candidate    (car live-intervals))
               (variable     (car candidate))
               (interval     (cdr candidate))
               (first-index  (car interval))
               (last-index   (cdr interval))
               (variable-use (mark-used-till variable-use variable last-index))
               (register     (find-available register-use first-index)); TODO: find available or predefined
               (recursion    (lambda (result register)
                               (linear-allocate (cdr live-intervals)
                                                (mark-used-till register-use register last-index)
                                                variable-use
                                                (assq-set result variable register))))]
          (if register
            (recursion result register)
            (let* [(spill-candidate (longest-use variable-use))
                   (register        (assq-ref result spill-candidate))]
              (recursion (assq-set result spill-candidate #f) register))))))
  (linear-allocate (sort-by live-intervals cadr); TODO: sort by predefined and cadr
                   (initial-register-use registers (predefined-variable-use live-intervals predefined) predefined)
                   '()
                   '()))

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (predefined '()))
  "Linear scan register allocation for a given program"
  (let* [(live         (live-analysis prog '())); TODO: specify return values here
         (all-vars     (variables prog))
         (intervals    (live-intervals live all-vars))
         (substitution (linear-scan-coloring intervals registers predefined))]
    (adjust-stack-pointer 8 (substitute-variables prog substitution))))

(ok (equal? '() (predefined-variable-use '((a . (0 . 3))) '()))
    "no predefined variables in use")
(ok (equal? '((a . 4)) (predefined-variable-use '((a . (0 . 3))) (list (cons 'a RAX))))
    "determine use of predefined variable")
(ok (equal? 0 (register-predefined-till '() '() RAX))
    "check that register is not predefined")
(ok (equal? 4 (register-predefined-till '((a . 4)) (list (cons 'a RAX)) RAX))
    "check availability point for register used by predefined variable")
(ok (equal? (list (cons RAX 0)) (initial-register-use (list RAX) '() '()))
    "initial availability points of registers are zero by default")
(ok (equal? (list (cons RAX 4)) (initial-register-use (list RAX) '((a . 4)) (list (cons 'a RAX))))
    "initial availability of register used by predefined variable allocation")
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
