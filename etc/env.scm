(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 optargs)
             (ice-9 curried-definitions)
             (aiscm util)
             (aiscm element)
             (aiscm pointer)
             (aiscm mem)
             (aiscm sequence)
             (aiscm jit)
             (aiscm op)
             (aiscm int))

(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))

(define prog (list (MOV a 1) (MOV b 2) (ADD b 3) (ADD a 4) (RET)))

(define vars (variables prog))
(define live (live-analysis prog))

(define start (map (lambda (v) (find (lambda (i) (memv v (list-ref live i))) (iota (length prog)))) vars))
(define end (map (lambda (v) (find (lambda (i) (memv v (list-ref live i))) (reverse (iota (length prog))))) vars))

(define intervals (zip vars start end))

(define ((adjacent intervals) node)
  (let [(interval (assq-ref intervals node))]
    (map car (filter (lambda (x) (or (>= (caddr x) (car interval))
                                     (<= (cadr x) (cadr interval))))
                     intervals))))

; (adjacent conflicts)
; (nodes graph)

; scm_display(scm_target, scm_current_output_port()); printf("\n");
