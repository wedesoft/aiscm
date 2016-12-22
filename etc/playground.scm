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

(define (parameters-to-spill parameters allocation)
  "Get a list of parameters which need spill code"
  (filter (compose not (cut assq-ref allocation <>)) (take-up-to parameters 6)))

(define (parameters-to-fetch parameters allocation)
  "Get a list of parameters which need to be fetched"
  (filter (cut assq-ref allocation <>) (drop-up-to parameters 6)))

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

(define (update-parameter-locations parameters locations initial-locations)
  "Generate spill code for spilled parameters"
  (map MOV (map (cut assq-ref locations <>) parameters) (map (cut assq-ref initial-locations <>) parameters)))

; TODO: create and use array of initial parameter locations

(let [(i (var <int>))
      (l (var <long>))]
  (ok (equal? '() (parameters-to-spill '(a) (list (cons 'a RAX))))
      "a parameter does not need spilling if a register was allocated for it")
  (ok (equal? '(a) (parameters-to-spill '(a) '()))
      "a parameter needs spilling if no register was allocated for it")
  (ok (equal? '() (parameters-to-fetch '(a) (list (cons 'a RAX))))
      "a parameter does not need fetching if it is one of the first six ones")
  (ok (equal? '(g) (parameters-to-fetch '(a b c d e f g) (list (cons 'g RAX))))
      "stack parameter with allocated register needs fetching")
  (ok (equal? '() (parameters-to-fetch '(a b c d e f g) '()))
      "stack parameter without allocated register does not need fetching")
  (ok (equal? '(a b c d e f) (parameters-to-spill '(a b c d e f g) '()))
      "only the first six parameters may need spilling")
  (ok (equal? '() (register-parameter-locations '()))
      "initial parameter locations for no parameters")
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
  (ok (equal? '() (update-parameter-locations '() '() '()))
      "write no parameter to stack")
  (ok (equal? (list (MOV (ptr <int> RSP -8) EDI))
              (update-parameter-locations (list i) (list (cons i (ptr <int> RSP -8))) (list (cons i EDI))))
      "write one parameter to stack"))

(run-tests)
