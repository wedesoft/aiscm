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

(define lst (unbuild (sequence <int>) (sequence 2 3 5)))

(define-method (build (type <meta<sequence<>>>) lst)
  (make (sequence <int>) #:value (make <mem> #:base (make-pointer (last lst)) #:size 12)
                         #:size (car lst)
                         #:strides (list (cadr lst))))


(define (content-s s) (list (parameter (make <long> #:value (dimension s)))
                            (parameter (make <long> #:value (stride s)))
                            (parameter (make <ulong> #:value (get (delegate (project s))))))); TODO: content of tensor expression

(define (pkg lst) (fold-right (cut native-call scm-cons <...>) (native-constant scm-eol) lst))

(define f (jit ctx (list (sequence <int>)) (lambda (s) (pkg (content-s s)))))

(diagnostics (f (seq <int> 2 3 5)))
(diagnostics (build (sequence <int>) (f (seq <int> 2 3 5))))
(diagnostics (build (sequence <int>) (unbuild (sequence <int>) (seq <int> 2 3 5))))

(run-tests)
