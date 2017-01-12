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


; remove predefinitions where register is blocked
; add code for moving values

(define (update-parameter-locations parameters locations offset)
  "Generate the required code to update the parameter locations according to the register allocation"
  (let [(initial-locations (map cdr (parameter-locations parameters offset)))
        (target-locations  (map (cut assq-ref locations <>) parameters))]
    (apply compact
      (map (lambda (parameter initial target)
             (let [(adapt (cut to-type (typecode parameter) <>))]
               (if (equal? initial target) #f (MOV (adapt target) (adapt initial)))))
           parameters initial-locations target-locations))))

(define* (linear-scan-allocate prog #:key (registers default-registers) (parameters '()) (blocked '()))
  "Linear scan register allocation for a given program"
  (let* [(live                 (live-analysis prog '())); TODO: specify return values here
         (temporary-variables  (temporary-variables prog))
         (intervals            (append (live-intervals live (variables prog))
                                       (unit-intervals temporary-variables)))
         (predefined-registers (register-parameter-locations (register-parameters parameters)))
         (parameters-to-move   (blocked-predefined predefined-registers intervals blocked))
         (remaining-predefines (non-blocked-predefined predefined-registers parameters-to-move))
         (stack-parameters     (stack-parameters parameters))
         (colors               (linear-scan-coloring intervals registers remaining-predefines blocked))
         (callee-saved         (used-callee-saved colors))
         (stack-offset         (* 8 (1+ (number-spilled-variables colors stack-parameters))))
         (parameter-offset     (+ stack-offset (* 8 (length callee-saved))))
         (stack-locations      (stack-parameter-locations stack-parameters parameter-offset))
         (allocation           (add-stack-parameter-information colors stack-locations))
         (temporaries          (temporary-registers allocation temporary-variables))
         (locations            (add-spill-information allocation 8 8))]
    (backup-registers
      callee-saved
      (adjust-stack-pointer
        stack-offset
        (append (update-parameter-locations parameters locations parameter-offset)
                ; TODO: correct order of copying parameters
                (append-map (cut replace-variables locations <...>) prog temporaries))))))

(define a (var <int>))

(ok (equal? (list (SUB RSP 8) (MOV EAX EDI) (MOV EDI EAX) (ADD RSP 8) (RET))
            (linear-scan-allocate (list (MOV EDI a) (RET))
                                  #:parameters (list a) #:registers (list RDI RAX RCX) #:blocked (list (cons RDI '(0 . 0)))))
    "move parameter variable into another location if the register is blocked")

(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (d (var <int>))
      (e (var <int>))
      (f (var <int>))
      (g (var <int>))
      (r (var <int>))]
  (ok (null? (update-parameter-locations '() '() 0))
      "no parameters to move arround")
  (ok (equal? (list (MOV (ptr <int> RSP -8) EDI))
              (update-parameter-locations (list a) (list (cons a (ptr <long> RSP -8))) 0))
      "spill a register parameter")
  (ok (equal? (list (MOV EAX (ptr <int> RSP 8)))
              (update-parameter-locations (list a b c d e f g)
                                          (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 RAX))
                                          0))
      "load a stack parameter")
  (ok (equal? (list (MOV EAX (ptr <int> RSP 24)))
              (update-parameter-locations (list a b c d e f g)
                                          (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 RAX))
                                          16))
      "load a stack parameter taking into account the stack pointer offset")
  (ok (null? (update-parameter-locations (list a b c d e f g)
                                         (map cons (list a b c d e f g) (list RDI RSI RDX RCX R8 R9 (ptr <long> RSP 24)))
                                         16))
      "leave parameter on stack"))

(run-tests)
