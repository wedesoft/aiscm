(use-modules (oop goops)
             (aiscm jit)
             (aiscm util)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 curried-definitions))



(define-class <cmd> ()
  (op #:init-keyword #:op #:getter get-op)
  (args #:init-keyword #:args #:getter get-args)
  (input #:init-keyword #:input #:getter get-input)
  (output #:init-keyword #:output #:getter get-output))
; TODO: just implement default (MOV a b)?
(define-method (MOV (a <symbol>) (b <symbol>))
  (make <cmd> #:op MOV #:args (list a b) #:input (list b) #:output (list a)))
(define-method (MOV (a <symbol>) imm)
  (make <cmd> #:op MOV #:args (list a imm) #:input '() #:output (list a)))
(define-method (ADD (a <symbol>) (b <symbol>))
  (make <cmd> #:op ADD #:args (list a b) #:input (delete-duplicates (list a b)) #:output (list a)))
(define-method (SUB (a <symbol>) (b <symbol>))
  (make <cmd> #:op SUB #:args (list a b) #:input (delete-duplicates (list a b)) #:output (list a)))
(define-method (SUB (a <symbol>) imm)
  (make <cmd> #:op SUB #:args (list a imm) #:input (list a) #:output (list a)))
(define-method (MUL (a <symbol>) (b <symbol>))
  (make <cmd> #:op MUL #:args (list a b) #:input (delete-duplicates (list a b)) #:output (list a)))
(define-method (CMP (a <symbol>) imm)
  (make <cmd> #:op CMP #:args (list a imm) #:input (list a) #:output (list)))
(define-method (NEG (a <symbol>))
  (make <cmd> #:op NEG #:args (list a) #:input (list a) #:output (list a)))

(define-method (get-input self) (if (equal? self (RET)) '(f) '()))
(define-method (get-output self) '())

; !!!
(define-method (subst (cmd <cmd>) alist)
  (apply (get-op cmd) (map (lambda (var) (or (assq-ref alist var) var)) (get-args cmd))))
(subst (NEG 'a) (list (cons 'a EAX)))

(define prog
  (list
    (NOP)
    (MOV 'f 0)
    (JMP 'condition)
    'loop
    (MOV 'y 'x)
    (MUL 'y 'y)
    (ADD 'f 'y)
    (SUB 'x 1)
    'condition
    (CMP 'x 0)
    (JNE 'loop)
    (RET)))

(define prog
  (list
    (MOV 'a 'x)
    (MOV 'b 'a)
    (MOV 'f 'b)
    (RET)))

(define (attach lst x) (reverse (cons x (reverse lst))))
(define (union . args) (apply lset-union (cons eq? args)))
(define (diff . args) (apply lset-difference (cons eq? args)))
(define (interface cmd) (union (get-output cmd) (get-input cmd)))
(define (inputs prog) (map get-input prog))
(define (outputs prog) (map get-output prog))
(define (index prog) (iota (length prog)))
(define (next prog) (attach (map list (cdr (index prog))) '()))
(define (jumps prog) (map (lambda (cmd) (if (is-a? cmd <jcc>) (list (assq-ref (labels prog) (get-target cmd))) '())) prog))
(define (labels prog) (filter (compose symbol? car) (map cons prog (index prog))))
(define (iteration live prog)
  (map (lambda (req ids prod) (union req (diff (apply union (map (cut list-ref live <>) ids)) prod)))
       (inputs prog)
       (map append (next prog) (jumps prog)); TODO: clear next for JMP
       (outputs prog)))
(define (iterate live prog)
  (let [(successor (iteration live prog))]
    (if (equal? live successor)
      (map union live (outputs prog))
      (iterate successor prog))))
(define (live prog) (iterate (map (const '()) prog) prog))
(for-each (lambda (cmd vars) (format #t "~a -> ~a~&" cmd vars)) prog (live prog))
(define (range lst) (cons (apply min lst) (apply max lst)))
(define (vars prog) (apply union (live prog)))
(define (intervals prog)
  (let* [(live     (live prog))
         (active   (lambda (var) (map (cut memv var <>) live)))
         (interval (lambda (var) (range (filter (cut list-ref (active var) <>) (index prog)))))]
    (map (lambda (var) (cons var (interval var))) (vars prog))))
(define (interfaces prog) (map interface prog))

(define (cost prog var) (length (filter (cut memv var <>) (append (inputs prog) (outputs prog)))))

(define (graph prog) (delete-duplicates (apply append (map product (live prog) (live prog)))))

(define (dot graph colors)
  (apply string-append
         (append (list "graph g {")
                 (map (lambda (color) (format #f " ~a [style=filled, fillcolor=~a];" (car color) (cdr color))) colors)
                 (map (lambda (edge) (format #f " ~a -- ~a;" (car edge) (cdr edge))) graph)
                 (list " }"))))
(define (graphviz graph colors) (system (format #f "echo '~a' | dot -Tpng | display -" (dot graph colors))))
(define (graphviz2 graph colors) (system (format #f "echo '~a' | dot -Tpng > process.png" (dot graph colors))))

(let [(graph '((b . a) (a . c) (d . c)))] (graphviz graph (coloring graph '(red green blue))))


; --------------

(let [(graph '((run . intr)
               (intr . runbl)
               (runbl . run)
               (run . kernel)
               (kernel . zombie)
               (kernel . sleep)
               (kernel . runmem)
               (sleep . swap)
               (swap . runswap)
               (runswap . new)
               (runswap . runmem)
               (new . runmem)
               (sleep . runmem)))]
  (graphviz graph (coloring graph '(red green blue yellow))))

; spill register

;(define-syntax env
;  (lambda (x)
;    (syntax-case x (call)
;      ((_)                (syntax (list)))
;      ((_ (call x) y ...) (syntax (cons x (env y ...)))); detect upper case identifier
;      ((_ (x ...) y ...)  (syntax (cons (env x ...) (env y ...))))
;      ((_ x y ...)        (syntax (cons (quote x) (env y ...))))
;      )))
;(define-method (op (x <integer>)) (env (MOV AX (call x)) (RET))); replace x?
;; (define-method (op (x_ <integer>)) (env [(x (reg (get-value x_)))] (MOV AX x) (RET)))
;(d (op 5))

; (define-method (ptr (type <meta<int<>>>) (v <var>)) ...)
; (define-method (MOV (v <var>) (u <var>)) (make <o> #:output (list v) #:input (list u)))
; (define-method (MOV (v <var>) imm) (make <o> #:output (list v)))
