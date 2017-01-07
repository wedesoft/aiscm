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
; pass blocked intervals to linear register coloring
; spill blocked predefined variables

(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (parameters '()))
  "Linear scan register allocation for a given program"
  (let* [(live                 (live-analysis prog '())); TODO: specify return values here
         (temporary-variables  (temporary-variables prog))
         (intervals            (append (live-intervals live (variables prog))
                                       (unit-intervals temporary-variables)))
         (predefined-registers (register-parameter-locations (register-parameters parameters)))
         (stack-parameters     (stack-parameters parameters))
         (colors               (linear-scan-coloring intervals registers predefined-registers '())); TODO: pass blocked intervals
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

(define r (var <byte>))
(define a (var <byte>))
(define b (var <byte>))

(define prog (list (MOV AL a) (CBW) (IDIV b) (MOV r AL) (RET)))

(linear-scan-allocate (list (MOV AL a) (CBW) (IDIV b) (MOV r AL) (RET)) #:registers (list RAX RCX RDX RSI))

(run-tests)
