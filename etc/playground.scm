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


(define a (var <int>)); TODO: rename "prog" in some places?
(define prog (list (MOV a EDI) (blocked RAX (MOV EAX a) (RET))))
(blocked-intervals prog)
(filter-blocks prog)
(define flat (flatten-code (filter-blocks prog)))
(variables flat)
(define intervals (live-intervals (live-analysis flat '()) (variables flat)))

(linear-scan-coloring (list (cons a '(0 . 1))) (list RAX RCX) '())

; TODO: remove blocked (overlap-interval)

(define (find-available-register availability first-index)
  "Find register available from the specified first program index onwards"
  (car (or (find (compose (cut <= <> first-index) cdr) availability) '(#f))))

(define (ignore-blocked-registers availability interval blocked)
  "Remove blocked registers from the availability list"
  (apply assq-remove availability (map car blocked)))

(define (linear-scan-coloring live-intervals registers predefined)
  "Linear scan register allocation based on live intervals"
  (define (linear-allocate live-intervals register-use variable-use allocation)
    (if (null? live-intervals)
        allocation
        (let* [(candidate    (car live-intervals))
               (variable     (car candidate))
               (interval     (cdr candidate))
               (first-index  (car interval))
               (last-index   (cdr interval))
               (variable-use (mark-used-till variable-use variable last-index))
               (register     (or (assq-ref predefined variable)
                                 (find-available-register register-use first-index)))
               (recursion    (lambda (allocation register)
                               (linear-allocate (cdr live-intervals)
                                                (mark-used-till register-use register last-index)
                                                variable-use
                                                (assq-set allocation variable register))))]
          (if register
            (recursion allocation register)
            (let* [(spill-candidate (longest-use variable-use))
                   (register        (assq-ref allocation spill-candidate))]
              (recursion (assq-set allocation spill-candidate #f) register))))))
  (linear-allocate (sort-live-intervals live-intervals (map car predefined))
                   (initial-register-use registers)
                   '()
                   '()))

(ok (equal? RAX (find-available-register (list (cons RAX 0)) 0))
    "first register available")
(ok (not (find-available-register (list (cons RAX 1)) 0))
    "first register not available")
(ok (equal? RAX (find-available-register (list (cons RAX 1)) 1))
    "first register available at a later point in time")
(ok (equal? RAX (find-available-register (list (cons RAX 0)) 1))
    "first register already available")
(ok (equal? RCX (find-available-register (list (cons RAX 3) (cons RCX 2)) 2))
    "second register is available")

(ok (equal? (list (cons RAX 2)) (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) '()))
    "do not ignore register if it is not blocked")
(ok (equal? '() (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) (list (cons RAX '(5 . 6)))))
    "ignore register for allocation if it is blocked")
(todo (equal? (list (cons RAX 2)) (ignore-blocked-registers (list (cons RAX 2)) '(3 . 5) (list (cons RAX '(6 . 8)))))
    "do not ignore register if it is blocked outside the specified interval")


(ok (equal? '() (linear-scan-coloring '() '() '()))
    "linear scan with no variables returns empty mapping")
(ok (equal? (list (cons 'a RAX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) '()))
    "allocate single variable")
(ok (equal? (list (cons 'a RAX) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 0)) (b . (1 . 1))) (list RAX RCX) '()))
    "reuse register with two variables")
(ok (equal? (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) '()))
    "allocate different registers for two variables conflicting at index 1")
(ok (equal? (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((b . (1 . 1)) (a . (0 . 1))) (list RAX RCX) '()))
    "sort live intervals by beginning of interval before performing linear-scan register allocation")
(ok (equal? (list (cons 'a RAX) (cons 'b RCX)) (linear-scan-coloring '((a . (0 . 0)) (b . (0 . 1))) (list RAX RCX) '()))
    "allocate different registers for two variables conflicting at index 0")
(ok (equal? (list (cons 'a RAX) (cons 'b #f)) (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 3))) (list RAX) '()))
    "mark last variable for spilling if it has a longer live interval")
(ok (equal? (list (cons 'a #f) (cons 'b RAX)) (linear-scan-coloring '((a . (0 . 3)) (b . (1 . 1))) (list RAX) '()))
    "mark first variable for spilling if it has a longer live interval")
(ok (equal? (list (cons 'a RCX)) (linear-scan-coloring '((a . (0 . 0))) (list RAX RCX) (list (cons 'a RCX))))
    "use predefined register for variable")
(ok (equal? (list (cons 'a RCX) (cons 'b RAX))
            (linear-scan-coloring '((a . (0 . 1)) (b . (1 . 1))) (list RAX RCX) (list (cons 'a RCX))))
    "predefined registers take priority over normal register allocations")

(todo (equal? (list (cons a RCX))
            (linear-scan-coloring (list (cons a '(0 . 1))) (list RAX RCX) (list (cons RAX '(1 . 2))) '()))
    "do not allocate register if it is blocked while the variable is live")

(run-tests)
