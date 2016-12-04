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

(define (labels prog)
  "Get positions of labels in program"
  (filter (compose symbol? car) (map cons prog (iota (length prog)))))

(define-method (next-indices labels cmd k)
  "Determine next program indices for a statement"
  (if (equal? cmd (RET)) '() (list (1+ k))))
(define-method (next-indices labels (cmd <jcc>) k)
  "Determine next program indices for a (conditional) jump"
  (let [(target (assq-ref labels (get-target cmd)))]
    (if (conditional? cmd) (list (1+ k) target) (list target))))

(define (live-analysis prog results)
  "Get list of live variables for program terminated by RET statement"
  (letrec* [(inputs    (map-if (cut equal? (RET) <>) (const results) input prog))
            (outputs   (map output prog))
            (indices   (iota (length prog)))
            (lut       (labels prog))
            (flow      (map (cut next-indices lut <...>) prog indices))
            (same?     (cut every (cut lset= equal? <...>) <...>))
            (track     (lambda (value)
                         (lambda (in ind out)
                           (union in (difference (apply union (map (cut list-ref value <>) ind)) out)))))
            (initial   (map (const '()) prog))
            (iteration (lambda (value) (map (track value) inputs flow outputs)))]
    (map union (fixed-point initial iteration same?) outputs)))

; TODO: sort intervals

(define (initial-register-use lst)
  "Initially all registers are available from index zero on"
  (map (cut cons <> 0) lst))

(define (find-available availability first-index)
  "Find element available from the specified first program index onwards"
  (car (or (find (compose (cut <= <> first-index) cdr) availability) '(#f))))

(define (mark-used-till availability element last-index)
  "Mark element in use up to specified index"
  (assq-set availability element (1+ last-index)))

(define (longest-use availability)
  "Select register blocking for the longest time as a spill candidate"
  (car (argmax cdr availability)))

(define (current-user allocation register)
  "Determine the variable which last allocated the register"
  (car (find (compose (cut eq? register <>) cdr) (reverse allocation))))

(define (linear-allocate live-intervals register-use variable-use . result)
  "recursively allocate registers"
  (if (null? live-intervals)
      result
      (let* [(candidate    (car live-intervals))
             (variable     (car candidate))
             (interval     (cdr candidate))
             (first-index  (car interval))
             (last-index   (cdr interval))
             (variable-use (mark-used-till variable-use variable last-index))
             (register     (find-available register-use first-index))
             (recursion    (lambda (result register)
                             (apply linear-allocate
                                     (cdr live-intervals)
                                     (mark-used-till register-use register last-index)
                                     variable-use
                                     (assq-set result variable register))))]
        (if register
          (recursion result register)
          (let* [(spill-candidate (longest-use variable-use))
                 (register (assq-ref result spill-candidate))]
            (recursion (assq-set result spill-candidate #f) register))))))

(define (linear-scan live-intervals registers)
  "linear scan register allocation"
  (linear-allocate live-intervals (initial-register-use registers) '()))

(ok (equal? '((a . 1) (b . 3)) (labels (list (JMP 'a) 'a (MOV AX 0) 'b (RET))))
    "'labels' should extract indices of labels")
(ok (equal? '(1) (next-indices '() (MOV CX 0) 0))
    "Get following indices for first statement in a program")
(ok (equal? '(2) (next-indices '() (MOV AX CX) 1))
    "Get following indices for second statement in a program")
(ok (equal? '() (next-indices '() (RET) 2))
    "RET statement should not have any following indices")
(ok (equal? '(2) (next-indices '((a . 2)) (JMP 'a) 0))
    "Get following indices for a jump statement")
(ok (equal? '(1 2) (next-indices '((a . 2)) (JNE 'a) 0))
    "Get following indices for a conditional jump")
(let [(a (var <int>))
      (b (var <int>))
      (c (var <int>))]
  (ok (equal? (list '() (list a) '()) (live-analysis (list 'x (MOV a 0) (RET)) '()))
      "Live-analysis for definition of unused variable")
  (ok (equal? (list (list a) (list a) (list b a) '()) (live-analysis (list (MOV a 0) (NOP) (MOV b a) (RET)) '()))
      "Live-analysis for definition and later use of a variable")
  (ok (equal? (list (list a) (list a) (list a) (list a) '())
              (live-analysis (list (MOV a 0) 'x (ADD a 1) (JE 'x) (RET)) '()))
      "Live-analysis with conditional jump statement")
  (ok (equal? (list (list a) (list a))
              (live-analysis (list (MOV a 0) (RET)) (list a)))
      "results should be propagated backwards from the return statement"))
(ok (equal? (list (cons RAX 0) (cons RCX 0)) (initial-register-use (list RAX RCX)))
    "initial availability points of registers")
(ok (equal? RAX (find-available (list (cons RAX 0)) 0))
    "first register available")
(ok (not (find-available (list (cons RAX 1)) 0))
    "first register not available")
(ok (equal? RAX (find-available (list (cons RAX 1)) 1))
    "first register available at a later point in time")
(ok (equal? RAX (find-available (list (cons RAX 0)) 1))
    "first register already available")
(ok (equal? RCX (find-available (list (cons RAX 3) (cons RCX 2)) 2))
    "second register is available")
(ok (equal? (list (cons RAX 4)) (mark-used-till (list (cons RAX 1)) RAX 3))
    "mark first register as used")
(ok (equal? (list (cons RAX 4) (cons RCX 5)) (mark-used-till (list (cons RAX 1) (cons RCX 5)) RAX 3))
    "keep track of unaffected registers")
(ok (equal? (list (cons RAX 1) (cons RCX 9)) (mark-used-till (list (cons RAX 1) (cons RCX 5)) RCX 8))
    "mark second register as used")
(ok (eq? 'a (current-user (list (cons 'a RCX)) RCX))
    "if only one variable is using the register, it is the last user")
(ok (eq? 'b (current-user (list (cons 'a RCX) (cons 'b RCX)) RCX))
    "if two variables are using the register, return the last one")
(ok (eq? 'b (current-user (list (cons 'a RCX) (cons 'b RCX)) RCX))
    "if two variables are using the register, return the last one")
(ok (eq? 'a (current-user (list (cons 'a RCX) (cons 'b RAX)) RCX))
    "ignore variables using other registers")
(ok (eq? RAX (longest-use (list (cons RAX 0))))
    "spill only register if there is only one candidate")
(ok (eq? RCX (longest-use (list (cons RAX 0) (cons RCX 1))))
    "spill second register if it is allocated for a longer interval")
(ok (eq? RAX (longest-use (list (cons RAX 1) (cons RCX 0))))
    "spill first register if it is allocated for a longer interval")
(ok (equal? '() (linear-scan '() '()))
    "linear scan with no variables returns empty mapping")
(ok (equal? (list (cons 'a RAX)) (linear-scan '((a . (0 . 0))) (list RAX)))
    "allocate single variable")
(ok (equal? (list (cons 'a RAX) (cons 'b RAX)) (linear-scan '((a . (0 . 0)) (b . (1 . 1))) (list RAX RCX)))
    "reuse register with two variables")
(ok (equal? (list (cons 'a RAX) (cons 'b RCX)) (linear-scan '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX)))
    "allocate different registers for two conflicting variables")
(ok (equal? (list (cons 'a RAX) (cons 'b RCX)) (linear-scan '((a . (0 . 0)) (b . (0 . 1))) (list RAX RCX)))
    "allocate different registers for two conflicting variables")
(ok (equal? (list (cons 'a RAX) (cons 'b #f)) (linear-scan '((a . (0 . 1)) (b . (1 . 3))) (list RAX)))
    "mark last variable for spilling if it has a longer live interval")
(ok (equal? (list (cons 'a #f) (cons 'b RAX)) (linear-scan '((a . (0 . 3)) (b . (1 . 1))) (list RAX)))
    "mark first variable for spilling if it has a longer live interval")

(run-tests)

(define v (var <int>))
(define prog (list (MOV v 42) (RET)))
(define intervals (live-intervals (live-analysis prog '()) (variables prog)))
(linear-scan intervals default-registers)
