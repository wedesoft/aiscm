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
   (delete-duplicates (lset-intersection eq? (map (compose (cut to-type <long> <>) cdr) allocation) callee-saved)))

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
         (stack-offset         (* 8 (1+ (number-spilled-variables colors stack-parameters))))
         (stack-locations      (stack-parameter-locations stack-parameters stack-offset))
         (allocation           (add-stack-parameter-information colors stack-locations))
         (temporaries          (temporary-registers allocation temporary-variables))
         (locations            (add-spill-information allocation 8 8))]
    (adjust-stack-pointer stack-offset
                          (append (update-parameter-locations parameters locations stack-offset)
                                  (append-map (lambda (cmd register) (replace-variables cmd locations register))
                                              prog
                                              temporaries)))))

(define a (var <int>))

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
(ok (equal? (list (SUB RSP 16) (MOV (ptr <long> RSP 8) RBX) (MOV EBX 1) (MOV RBX (ptr <long> RSP 8)) (ADD RSP 16))
            (linear-scan-allocate (list (MOV a 1) (RET)) #:registers (list RBX RAX)))
    "save callee-saved registers")

(run-tests)
