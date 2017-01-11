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

(define (blocked-predefined predefined intervals blocked)
  "Get blocked predefined registers"
  (filter (compose not null? (overlap-interval blocked) (cut assq-ref intervals <>) car) predefined))

(define (move-blocked-predefined blocked-predefined)
  "Generate code for blocked predefined variables"
  (map (compose MOV car+cdr) blocked-predefined))

(define (non-blocked-predefined predefined blocked-predefined)
  "Compute the set difference of the predefined variables and the variables with blocked registers"
  (lset-difference equal? predefined blocked-predefined))

(define (parameter-locations parameters offset)
  "return association list with default locations for the method parameters"
  (let [(register-parameters (register-parameters parameters))
        (stack-parameters    (stack-parameters parameters))]
    (append (register-parameter-locations register-parameters)
            (stack-parameter-locations stack-parameters offset))))

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

(ok (null? (blocked-predefined '() '() '()))
    "no predefined variables")
(ok (equal? (list (cons 'a RDI))
            (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(1 . 3)))))
    "detect predefined variable with blocked register")
(ok (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) '()))
    "ignore predefined variables if no registers are blocked")
(ok (null? (blocked-predefined (list (cons 'a RDI)) '((a . (0 . 2))) (list (cons RDI '(3 . 4)))))
    "ignore predefined variables if the associated register is not blocked")

(define a (var <int>))

(ok (null? (move-blocked-predefined '()))
    "no predefined variables with blocked registers to move")
(ok (equal? (list (MOV a RAX)) (move-blocked-predefined (list (cons a RAX))))
    "copy variable from blocked register")

(ok (equal? (list (cons 'a RDI)) (non-blocked-predefined (list (cons 'a RDI)) '()))
    "no predefinitions to discard")
(ok (equal? '() (non-blocked-predefined (list (cons 'a RDI)) (list (cons 'a RDI))))
    "discard predefined variables which are blocked")
(ok (equal? (list (cons 'b RSI)) (non-blocked-predefined (list (cons 'a RDI) (cons 'b RSI)) (list (cons 'a RDI))))
    "only discard predefined variables which are blocked")

(ok (equal? (list (SUB RSP 8) (MOV EAX EDI) (MOV EDI EAX) (ADD RSP 8) (RET))
            (linear-scan-allocate (list (MOV EDI a) (RET))
                                  #:parameters (list a) #:registers (list RDI RAX RCX) #:blocked (list (cons RDI '(0 . 0)))))
    "move parameter variable into another location if the register is blocked")

(ok (null? (parameter-locations '() 0))
    "parameter locations for empty set of parameters")
(ok (equal? (list (cons 'a RDI) (cons 'b RSI)) (parameter-locations '(a b) 0))
    "parameter location for first parameter")
(ok (equal? (list (cons 'a RDI) (cons 'b RSI) (cons 'c RDX) (cons 'd RCX) (cons 'e R8) (cons 'f R9)
                  (cons 'g (ptr <long> RSP 8)) (cons 'h (ptr <long> RSP 16)))
            (parameter-locations '(a b c d e f g h) 0))
    "parameter locations for register and stack parameters")
(ok (equal? (list (cons 'a RDI) (cons 'b RSI) (cons 'c RDX) (cons 'd RCX) (cons 'e R8) (cons 'f R9)
                  (cons 'g (ptr <long> RSP 24)) (cons 'h (ptr <long> RSP 32)))
            (parameter-locations '(a b c d e f g h) 16))
    "parameter locations for register and stack parameters")

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
