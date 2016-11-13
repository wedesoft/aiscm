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

(define (pkg lst) (fold-right (cut native-call scm-cons <...>) (native-constant scm-eol) lst))

(define f (jit ctx (list (sequence <int>)) (lambda (s) (pkg (content (type s) s)))))

(diagnostics (f (seq <int> 2 3 5)))
(diagnostics (build (sequence <int>) (f (seq <int> 2 3 5))))
(diagnostics (build (sequence <int>) (unbuild (sequence <int>) (seq <int> 2 3 5))))

(define (content-vars args) (map get (append-map content (map class-of args) args)))

(define (assemble return-args args instructions)
  "Determine result variables, argument variables, and instructions"
  (list (content-vars return-args) (content-vars args) (attach instructions (RET))))

(define (package-return-content value)
  "Generate code to package parameter VALUE in a Scheme list"
  (fold-right (cut native-call scm-cons <...>) (native-constant scm-eol) (content (type value) value)))

(define (generate-return-code args expr)
  (let [(result (parameter (type expr)))
        (retval (skeleton <obj>))]
    (list (list retval)
          args
          (insert-intermediate expr
                               (parameter (type expr))
                               (lambda (intermediate) (code (parameter retval) (package-return-content intermediate)))))))

(define context (make <context>))
(define classes (list (sequence <int>)))
(define proc identity)
(define vars        (map skeleton classes))
(define expr        (apply proc (map parameter vars)))
(define result-type (type expr))
(define sequence?   (is-a? result-type <meta<sequence<>>>))
(define result      (skeleton result-type))
(define args        (if sequence? (cons result vars) vars))
(define types       (map class-of args))

(append-map (code (parameter result) expr) (package-return-content (parameter result)))

(content (parameter (sequence <int>)))

;(define code        (if sequence?
;               (asm context
;                    <null>
;                    (map typecode (content-vars args))
;                    (apply virtual-variables (assemble '() args (code (parameter result) expr))))
;               (asm context
;                    <ulong>
;                    (map typecode (content-vars args))
;                    (apply virtual-variables (apply assemble (generate-return-code args expr))))))
;(fun         (lambda header (apply code (append-map unbuild types header))))

(run-tests)
