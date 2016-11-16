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

(define r (parameter <ulong>))
(define s (parameter <long>))

(asm ctx <ulong> (list <long>)
  (apply virtual-variables
    (assemble
      (list (delegate r)) (list (delegate s))
        (append (code r (native-call scm-gc-malloc s))))))


(define s (parameter (sequence <int>)))

(define o (parameter <obj>))
(define p (parameter <ulong>))
(define d (parameter <long>))
(define n (parameter <long>))

; TODO: compose sequence parameter
; TODO: construct sequence given shape information

(define s (skeleton (sequence <ubyte>)))

(define t (parameter s))

(make (sequence <ubyte>) #:shape (shape s) #:strides (strides s) #:value (value s))

;(define (construct-sequence typecode shape) )

(build
  (sequence <ubyte>)
  (address->scm
    (apply (asm ctx <ulong> (list <long> <long> <ulong>)
       (apply virtual-variables
         (assemble (list (delegate o)) (content (sequence <ubyte>) s)
           (append (code n (parameter (make <long> #:value (car (shape s)))))
                   (code p (native-call scm-gc-malloc-pointerless n))
                   (code d (native-constant (native-value <int> 1)))
                   (code o (build-list n d p)))))) (unbuild (sequence <ubyte>) (seq 2 3 5)))))

((jit ctx (list <int> <int>) build-list) 2 3)

(build (sequence <ubyte>)
 ((jit ctx (list <long>)
   (lambda (n) (build-list n (native-constant (native-value <int> 1)) (native-call scm-gc-malloc-pointerless n)))) 3))

(run-tests)
