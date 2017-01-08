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


; save callee-saved registers
; spill blocked predefined variables (or remove predefinition and assign from register while blocking it)
(define (used-callee-saved allocation)
   "Return the list of callee saved registers in use"
   (delete-duplicates (lset-intersection eq? (map (cut to-type <long> <>) (apply compact (map cdr allocation))) callee-saved)))

(define (backup-registers registers code)
  "Store register content on stack and restore it after executing the code"
  (append (map (cut PUSH <>) registers) (all-but-last code) (map (cut POP <>) (reverse registers)) (list (RET))))

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (parameters '())
                                          (blocked '()))
  "Linear scan register allocation for a given program"
  (let* [(live                 (live-analysis prog '())); TODO: specify return values here
         (temporary-variables  (temporary-variables prog))
         (intervals            (append (live-intervals live (variables prog))
                                       (unit-intervals temporary-variables)))
         (predefined-registers (register-parameter-locations (register-parameters parameters)))
         (stack-parameters     (stack-parameters parameters))
         (colors               (linear-scan-coloring intervals registers predefined-registers blocked))
         (callee-saved         (used-callee-saved colors))
         (stack-offset         (* 8 (1+ (number-spilled-variables colors stack-parameters))))
         (parameter-offset     (+ stack-offset (* 8 (length callee-saved))))
         (stack-locations      (stack-parameter-locations stack-parameters parameter-offset)); TODO: refactor stack-parameter-locations?
         (allocation           (add-stack-parameter-information colors stack-locations))
         (temporaries          (temporary-registers allocation temporary-variables))
         (locations            (add-spill-information allocation 8 8))]
    (backup-registers
      callee-saved
      (adjust-stack-pointer
        stack-offset
        (append (update-parameter-locations parameters locations parameter-offset)
                (append-map (lambda (cmd register) (replace-variables cmd locations register))
                            prog
                            temporaries))))))

(define a (var <int>))
(define b (var <int>))
(define c (var <int>))
(define d (var <int>))
(define e (var <int>))
(define f (var <int>))
(define g (var <int>))
(define r (var <int>))

(ok (equal? '() (used-callee-saved '()))
    "no registers in use")
(ok (equal? (list RBX) (used-callee-saved (list (cons 'a RBX))))
    "callee saved register in use")
(ok (equal? (list RBX) (used-callee-saved (list (cons 'a EBX))))
    "callee saved integer register in use")
(ok (equal? (list RBX) (used-callee-saved (list (cons 'a RBX) (cons 'b RBX))))
    "remove duplicate registers")
(ok (equal? '() (used-callee-saved (list (cons 'a RAX))))
    "ignore caller saved register")
(ok (equal? '()  (used-callee-saved (list (cons 'a #f))))
    "ignore variables without allocated register")
(ok (equal? (list (PUSH RBX) (NOP) (POP RBX) (RET)) (backup-registers (list RBX) (list (NOP) (RET))))
    "backup one register")
(ok (equal? (list (PUSH RBX) (PUSH RBP) (NOP) (POP RBP) (POP RBX) (RET)) (backup-registers (list RBX RBP) (list (NOP) (RET))))
    "backup two registers")
(ok (equal? (list (PUSH RBX) (SUB RSP 8) (MOV EBX 1) (ADD RSP 8) (POP RBX) (RET))
            (linear-scan-allocate (list (MOV a 1) (RET)) #:registers (list RBX RAX)))
    "save callee-saved registers")

(ok (equal? (list (PUSH RBX) (SUB RSP 8) (MOV EBX (ptr <int> RSP 24)) (MOV EBX 42) (ADD RSP 8) (POP RBX) (RET))
            (linear-scan-allocate (list (MOV g 42) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
    "add offset for callee-saved parameters when fetching stack parameters")
(ok (equal? (list (PUSH RBX) (SUB RSP 8) (MOV EBX EAX) (MOV (ptr <int> RSP 24) EBX) (ADD RSP 8) (POP RBX) (RET))
            (linear-scan-allocate (list (MOV g r) (RET)) #:parameters (list a b c d e f g) #:registers (list RBX RAX)))
    "add offset for callee-saved parameters when using stack parameters")

(run-tests)
