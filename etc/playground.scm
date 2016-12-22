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

(define (initial-parameter-locations parameters)
  "Create an association list with the initial parameter locations"
  (map (lambda (parameter register)
         (cons parameter (to-type (typecode parameter) register)))
       parameters
       (list RDI RSI RDX RCX R8 R9)))

(define (write-spilled-parameters parameters locations)
  "Generate spill code for spilled parameters"
  (map MOV (map (cut assq-ref locations <>) parameters) parameters))

(define (fetch-stack-parameters parameters locations)
  "Generate fetch code for stack parameters"
  '())

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
  (ok (equal? '() (initial-parameter-locations '()))
      "initial parameter locations for no parameters")
  (ok (equal? (list (cons i EDI)) (initial-parameter-locations (list i)))
      "initial parameter location for one integer parameter")
  (ok (equal? (list (cons l RDI)) (initial-parameter-locations (list l)))
      "initial parameter location for one long integer parameter")
  (ok (equal? (list RDI RSI RDX RCX R8 R9) (map cdr (initial-parameter-locations (make-list 6 l))))
      "initial parameter locations for first six parameters")
  (ok (equal? '() (write-spilled-parameters '() '()))
      "write no parameter to stack")
  (ok (equal? (list (MOV (ptr <int> RSP -8) i)) (write-spilled-parameters (list i) (list (cons i (ptr <int> RSP -8)))))
      "write one parameter to stack")
  (ok (equal? '() (fetch-stack-parameters '() '()))
      "no parameters to fetch"))

(run-tests)
