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

(define (number-spilled-variables allocation)
  "Count the number of spilled variables"
  (length (unallocated-variables allocation)))

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (predefined '()))
  "Linear scan register allocation for a given program"
  (let* [(live         (live-analysis prog '())); TODO: specify return values here
         (all-vars     (variables prog))
         (intervals    (live-intervals live all-vars))
         (allocation   (linear-scan-coloring intervals registers predefined)); TODO: allocate temporary for each statement
         (stack-offset (* 8 (1+ (number-spilled-variables allocation))))
         (locations    (add-spill-information allocation 8 8))]
    (adjust-stack-pointer stack-offset (concatenate (map (cut replace-variables <> locations RAX) prog)))))

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (x (var <sint>))]
  (ok (eqv? 0 (number-spilled-variables '()))
      "count zero spilled variables")
  (ok (eqv? 1 (number-spilled-variables '((a . #f))))
      "count one spilled variable")
  (ok (eqv? 0 (number-spilled-variables (list (cons a RAX))))
      "ignore allocated variables when counting spilled variables")
  (ok (equal? (list (SUB RSP 16)
                    (MOV EAX 1)
                    (MOV (ptr <int> RSP 8) EAX)
                    (MOV ESI 2)
                    (ADD ESI 3)
                    (MOV EAX (ptr <int> RSP 8))
                    (ADD EAX 4)
                    (MOV (ptr <int> RSP 8) EAX)
                    (ADD RSP 16)
                    (RET))
              (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET))
                                    #:registers (list RSI)))
      "'linear-scan-allocate' should spill variables"))

(run-tests)
