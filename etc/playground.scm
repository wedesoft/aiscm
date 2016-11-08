(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (system foreign)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm rgb)
             (aiscm obj)
             (aiscm asm)
             (aiscm jit)
             (aiscm method)
             (aiscm util)
             (guile-tap))

(define ctx (make <context>))

;(address->scm (apply (asm ctx <ulong> (map typecode (content-vars args)) (apply virtual-variables (apply assemble (generate-return-code (list o) expr)))) (unbuild (typecode o) (rgb 2 3 5))))

(define f (jit ctx (list <ulong>) (cut native-call scm-gc-malloc-pointerless <>)))

(define (g size) (f size))

(define-method (build (type <meta<pointer<>>>) lst) (make-pointer (car lst))
(list (pointer-address (get-memory self))))

(diagnostics (build (pointer <int>) (list (g 4))))

(diagnostics (unbuild (pointer <int>) (make (pointer <int>))))

(run-tests)
