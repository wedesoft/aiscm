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

(define (register-parameters parameters)
   "Return the parameters which are stored in registers according to the x86 ABI"
   (take-up-to parameters 6))

(define (stack-parameters parameters)
   "Return the parameters which are stored on the stack according to the x86 ABI"
   (drop-up-to parameters 6))

(define (parameters-to-spill register-parameters allocation)
  "Get a list of parameters which need spill code"
  (filter (compose not (cut is-a? <> <register>) (cut assq-ref allocation <>)) register-parameters))

(define (parameters-to-fetch stack-parameters allocation)
  "Get a list of parameters which need to be fetched"
  (filter (compose (cut is-a? <> <register>) (cut assq-ref allocation <>)) stack-parameters))

(define (register-parameter-locations parameters)
  "Create an association list with the initial parameter locations"
  (map (lambda (parameter register) (cons parameter (to-type (typecode parameter) register)))
       parameters
       (list RDI RSI RDX RCX R8 R9)))

(define (stack-parameter-locations parameters offset)
  "Determine initial locations of stack parameters"
  (map (lambda (parameter index) (cons parameter (ptr (typecode parameter) RSP index)))
       parameters
       (iota (length parameters) (+ 8 offset) 8)))

(define (move-parameters parameters allocation initial-locations)
  "Generate spill code for spilled parameters"
  (map MOV (map (cut assq-ref allocation <>) parameters) (map (cut assq-ref initial-locations <>) parameters)))

(define (update-parameter-locations parameters allocation offset)
  "Generate the required code to update the parameter locations according to the register allocation"
  (let [(register-parameters (register-parameters parameters))
        (stack-parameters    (stack-parameters parameters))]
    (append (move-parameters (parameters-to-spill register-parameters allocation)
                             allocation
                             (register-parameter-locations register-parameters))
            (move-parameters (parameters-to-fetch stack-parameters allocation)
                             allocation
                             (stack-parameter-locations stack-parameters offset)))))

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
(ok (equal? '(a) (parameters-to-spill '(a) '()))
    "a parameter needs spilling if no register was allocated for it")
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
  (ok (equal? (list (cons i EDI)) (register-parameter-locations (list i)))
      "initial parameter location for one integer parameter")
  (ok (equal? (list (cons l RDI)) (register-parameter-locations (list l)))
      "initial parameter location for one long integer parameter")
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
              (move-parameters (list i) (list (cons i (ptr <int> RSP -8))) (list (cons i EDI))))
      "write one parameter to stack"))
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))
      (d (var <int>))
      (e (var <int>))
      (f (var <int>))
      (g (var <int>))]
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
      "do nothing if only the first six parameters are in registers"))

(run-tests)
