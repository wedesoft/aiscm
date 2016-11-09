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

(define ctx (make <context>))

;(unbuild (sequence <int>) (sequence 2 3 5))
;
;(define s (parameter (sequence <ubyte>)))
;
;(define (content-s s) (list (parameter (make <int> #:value (dimension s))) (parameter (make <int> #:value (stride s))) (project s)))
;
;(define (pkg lst) (fold-right (cut native-call scm-cons <...>) (native-constant scm-eol) lst))
;
;(define f (jit ctx (list (sequence <int>)) (lambda (s) (pkg (content-s s)))))

(jit ctx (list (sequence <int>)) identity)

(define context ctx)
(define classes (list (sequence <int>)))
(define proc identity)

(define vars        (map skeleton classes))
(define expr        (apply proc (map parameter vars)))
(define result-type (type expr))
(define sequence?   (is-a? result-type <meta<sequence<>>>))
(define result      (skeleton result-type))
(define args        (if sequence? (cons result vars) vars))
(define types       (map class-of args))

(asm context
                             <null>
                             (map typecode (content-vars args))
                             (apply virtual-variables (assemble '() args (code (parameter result) expr))))

(define (content-vars args) (append-map content (map class-of args) (map get args)))

(define return-args '())
(define instructions (code (parameter result) expr))

(content-vars args)

(run-tests)
