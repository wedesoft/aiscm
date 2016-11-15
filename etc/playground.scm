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

(define p (parameter <ulong>))
(define d (parameter <long>))
(define s (parameter <long>))

(jit ctx (list <ulong>) (lambda (size) (native-call scm-gc-malloc-pointerless size)))

(asm ctx <ulong> (list <long>)
  (apply virtual-variables
    (assemble
      (list (delegate r)) (list (delegate s))
        (append (code r (native-call scm-gc-malloc s))))))

(build
  (sequence <ubyte>)
  (address->scm
    ((asm ctx <ulong> (list <ulong>)
       (apply virtual-variables
         (assemble (list (delegate o)) (list (delegate s))
           (append (code p (native-call scm-gc-malloc-pointerless s))
                   (code d (native-constant (native-value <int> 1)))
                   (code o (native-call scm-cons s
                             (native-call scm-cons d
                               (native-call scm-cons p (native-constant scm-eol))))))))) 3)))

; TODO: command to pack list (instead of content)

(run-tests)
