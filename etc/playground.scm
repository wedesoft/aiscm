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

; TODO: loading of stack parameters
; TODO: use stack location as spill locations for stack parameters
; determine offset in add-spill-information
; assign-spill-locations
; add-spill-information

(define (number-spilled-variables allocation); TODO: do not count stack parameters!
  "Count the number of spilled variables"
  (length (unallocated-variables allocation)))

(define (register-parameters parameters)
   "Return the parameters which are stored in registers according to the x86 ABI"
   (take-up-to parameters 6))

(define (stack-parameters parameters)
   "Return the parameters which are stored on the stack according to the x86 ABI"
   (drop-up-to parameters 6))

(define (parameters-to-spill register-parameters locations)
  "Get a list of parameters which need spill code"
  (filter (compose (cut is-a? <> <address>) (cut assq-ref locations <>)) register-parameters))

(define (parameters-to-fetch stack-parameters locations)
  "Get a list of parameters which need to be fetched"
  (filter (compose (cut is-a? <> <register>) (cut assq-ref locations <>)) stack-parameters))

(define (register-parameter-locations parameters)
  "Create an association list with the initial parameter locations"
  (map cons parameters (list RDI RSI RDX RCX R8 R9)))

(define (stack-parameter-locations parameters offset)
  "Determine initial locations of stack parameters"
  (map (lambda (parameter index) (cons parameter (ptr (typecode parameter) RSP index)))
       parameters
       (iota (length parameters) (+ 8 offset) 8)))

(define (add-stack-parameter-information allocation stack-parameter-locations)
   "Add the stack location for stack parameters which do not have a register allocated"
   (map (lambda (variable location) (cons variable (or location (assq-ref stack-parameter-locations variable))))
        (map car allocation)
        (map cdr allocation)))

(define (move-parameters parameters locations initial-locations)
  "Generate spill code for spilled parameters"
  (let [(adapt (lambda (alist parameter) (to-type (typecode parameter) (assq-ref alist parameter))))]
    (map MOV (map (cut adapt locations <>) parameters) (map (cut adapt initial-locations <>) parameters))))

(define (update-parameter-locations parameters locations offset)
  "Generate the required code to update the parameter locations according to the register allocation"
  (let [(register-parameters (register-parameters parameters))
        (stack-parameters    (stack-parameters parameters))]
    (append (move-parameters (parameters-to-spill register-parameters locations)
                             locations
                             (register-parameter-locations register-parameters))
            (move-parameters (parameters-to-fetch stack-parameters locations)
                             locations
                             (stack-parameter-locations stack-parameters offset)))))


(define* (linear-scan-allocate prog #:key (registers default-registers)
                                          (parameters '()))
  "Linear scan register allocation for a given program"
  (let* [(live                 (live-analysis prog '())); TODO: specify return values here
         (temporary-variables  (temporary-variables prog))
         (intervals            (append (live-intervals live (variables prog))
                                       (unit-intervals temporary-variables)))
         (predefined-registers (register-parameter-locations (register-parameters parameters)))
         (stack-locations      (stack-parameter-locations (stack-parameters parameters) 24)); TODO: correct offset
         (colors               (linear-scan-coloring intervals registers predefined-registers))
         (stack-offset         (* 8 (1+ (number-spilled-variables colors))))
         (allocation           (add-stack-parameter-information colors stack-locations))
         (temporaries          (temporary-registers allocation temporary-variables))
         (locations            (add-spill-information allocation 8 8))]
    (adjust-stack-pointer stack-offset
                          (append (update-parameter-locations parameters locations stack-offset)
                                  (append-map (lambda (cmd register) (replace-variables cmd locations register))
                                              prog
                                              temporaries)))))



; TODO: create and use array of initial parameter locations

(ok (equal? '(a b c) (register-parameters '(a b c)))
    "the first parameters are register parameters")
(ok (equal? '(a b c d e f) (register-parameters '(a b c d e f g)))
    "only the first six parameters are register parameters")
(ok (equal? '() (stack-parameters '(a b c)))
    "the first few parameters are not stored on the stack")
(ok (equal? '(g h) (stack-parameters '(a b c d e f g h)))
    "appart from the first six parameters, all parameters are stored on the stack")
(ok (equal? '() (parameters-to-spill '(a) (list (cons 'a RAX))))
    "a parameter does not need spilling if a register was allocated for it")
(ok (equal? '() (parameters-to-spill '(a) '()))
    "a parameter does not need spilling if it does not need an allocation")
(ok (equal? '(a) (parameters-to-spill '(a) (list (cons 'a (ptr <int> RSP -8)))))
    "a parameter needs spilling if a stack location was allocated for it")
(ok (equal? '(g) (parameters-to-fetch '(g) (list (cons 'g RAX))))
    "stack parameter with allocated register needs fetching")
(ok (equal? '() (parameters-to-fetch '(g) '()))
    "stack parameter without allocated register does not need fetching")
(ok (equal? '() (parameters-to-fetch '(g) (list (cons 'g (ptr <int> RSP -8)))))
    "stack parameter with stack location does not need fetching")
(ok (equal? '() (register-parameter-locations '()))
    "initial parameter locations for no parameters")
(let [(i (var <int>))
      (l (var <long>))]
  (ok (equal? (list (cons i RDI)) (register-parameter-locations (list i)))
      "initial parameter location for one parameter")
  (ok (equal? (list RDI RSI RDX RCX R8 R9) (map cdr (register-parameter-locations (make-list 6 l))))
      "initial parameter locations for first six parameters")
  (ok (equal? '() (stack-parameter-locations '() 0))
      "initial stack parameter locations for no parameters")
  (ok (equal? (list (cons i (ptr <int> RSP 8))) (stack-parameter-locations (list i) 0))
      "initial parameter location of an integer stack parameter")
  (ok (equal? (list (cons l (ptr <long> RSP 8))) (stack-parameter-locations (list l) 0))
      "initial parameter location of a long integer stack parameter")
  (ok (equal? (list (ptr <int> RSP 8) (ptr <int> RSP 16)) (map cdr (stack-parameter-locations (list i i) 0)))
      "parameter locations of two stack parameters")
  (ok (equal? (list (ptr <int> RSP 24) (ptr <int> RSP 32)) (map cdr (stack-parameter-locations (list i i) 16)))
      "take stack offset into account when determining stack parameter locations")
  (ok (equal? '() (move-parameters '() '() '()))
      "write no parameter to stack")
  (ok (equal? (list (MOV (ptr <int> RSP -8) EDI))
              (move-parameters (list i) (list (cons i (ptr <int> RSP -8))) (list (cons i RDI))))
      "write one parameter to stack")
  (ok (equal? '() (add-stack-parameter-information '() '()))
      "no stack location required")
  (ok (equal? (list (cons i (ptr <int> RSP 8)))
              (add-stack-parameter-information (list (cons i #f)) (list (cons i (ptr <int> RSP 8)))))
      "use stack location for register spilling")
  (ok (equal? (list (cons i RAX))
              (add-stack-parameter-information (list (cons i RAX)) (list (cons i (ptr <int> RSP 8)))))
      "do not use stack location if register already has a location allocated"))
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (d (var <int>))
      (e (var <int>))
      (f (var <int>))
      (g (var <int>))
      (r (var <int>))]
  (ok (equal? '() (update-parameter-locations '() '() 0))
      "no parameters to move arround")
  (ok (equal? (list (MOV (ptr <int> RSP -8) EDI))
              (update-parameter-locations (list a) (list (cons a (ptr <int> RSP -8))) 0))
      "spill a register parameter")
  (ok (equal? (list (MOV EAX (ptr <int> RSP 8)))
              (update-parameter-locations (list a b c d e f g)
                                          (map cons (list a b c d e f g) (list EDI ESI EDX ECX R8D R9D EAX))
                                          0))
      "load a stack parameter")
  (ok (equal? (list (MOV EAX (ptr <int> RSP 24)))
              (update-parameter-locations (list a b c d e f g)
                                          (map cons (list a b c d e f g) (list EDI ESI EDX ECX R8D R9D EAX))
                                          16))
      "load a stack parameter taking into account the stack pointer offset")
  (ok (equal? '()
              (update-parameter-locations (list a b c d e f g)
                                          (map cons (list a b c d e f) (list EDI ESI EDX ECX R8D R9D))
                                          0))
      "do nothing if only the first six parameters are in registers")


  (ok (equal? (list (SUB RSP 8) (MOV EAX 42) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 42) (RET))))
      "Allocate a single register")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 42) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 42) (RET)) #:registers (list RCX RDX)))
      "Allocate a single register using custom list of registers")
  (ok (equal? (list (SUB RSP 8) (MOV ECX 1) (MOV EDX 2) (ADD ECX EDX) (MOV EAX ECX) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV a 1) (MOV b 2) (ADD a b) (MOV c a) (RET))))
      "Allocate multiple registers")
  (ok (equal? (list (SUB RSP 8) (MOV EDX 1) (ADD EDX EDI) (MOV EDI EDX) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV b 1) (ADD b a) (MOV c b) (RET))
                                    #:parameters (list a) #:registers (list RDI RSI RDX RCX)))
      "Register allocation with predefined parameter register")
  (ok (equal? (list (SUB RSP 16) (MOV (ptr <int> RSP 8) EDI) (MOV EDI 1) (ADD EDI (ptr <int> RSP 8)) (ADD RSP 16) (RET))
              (linear-scan-allocate (list (MOV b 1) (ADD b a) (RET)) #:parameters (list a) #:registers (list RDI RSI)))
      "Spill register parameter")
  (ok (equal? (list (SUB RSP 8) (MOV EDI (ptr <int> RSP 16)) (MOV EAX EDI) (ADD RSP 8) (RET))
              (linear-scan-allocate (list (MOV r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX RDI RSI RAX)))
      "Fetch register parameter")
  (ok (eqv? 0 (number-spilled-variables '()))
      "count zero spilled variables")
  (ok (eqv? 1 (number-spilled-variables '((a . #f))))
      "count one spilled variable")
  (ok (eqv? 0 (number-spilled-variables (list (cons a RAX))))
      "ignore allocated variables when counting spilled variables")
  (skip (equal? (list (SUB RSP 16) (MOV EAX 0) (MOV (ptr <int> RSP 8) EAX) (MOV EAX (ptr <int> RSP 8)) (ADD EAX (ptr <int> RSP 24)) (MOV (ptr <int> RSP 8) EAX) (ADD RSP 16) (RET))
              (linear-scan-allocate (list (MOV r 10) (ADD r g) (RET)) #:parameters (list a b c d e f g) #:registers (list RAX)))
      "Reuse stack location for spilled stack parameters"))

(run-tests)
