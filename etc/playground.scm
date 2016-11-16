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

(define o (parameter <obj>))
(define r (parameter <ulong>))
(define s (parameter <long>))

(asm ctx <ulong> (list <long>)
  (apply virtual-variables
    (assemble
      (list (delegate r)) (list (delegate s))
        (append (code r (native-call scm-gc-malloc s))))))

(define p (parameter <ulong>))
(define d (parameter <long>))
(define n (parameter <long>))

(define s (skeleton (sequence <ubyte>)))

(build
  (sequence <ubyte>)
  (address->scm
    ((asm ctx <ulong> (list <ulong>)
       (apply virtual-variables
         (assemble (list (delegate o)) (list (delegate n))
           (append (code p (native-call scm-gc-malloc-pointerless n))
                   (code d (native-constant (native-value <int> 1)))
                   (code o (build-list n d p)))))) 3)))

(run-tests)
