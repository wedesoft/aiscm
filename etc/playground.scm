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

(define one (native-constant (native-value <long> 1)))

; TODO: strides of parameter should be a list of parameters
; TODO: shape of function

(define s (skeleton (sequence <int>)))
(define t (parameter s))

(define o (parameter <obj>))

(define u (parameter (sequence <int>)))

; TODO: implement "size-of" for parameters

; TODO: create value and initialisation code
(define (construct-value retval expr)
  (append (append-map code (shape retval) (shape expr))
          (code (car (content (pointer <int>) (project retval)))
                (native-call scm-gc-malloc-pointerless (size-of retval)))
          (append-map code (strides retval) (default-strides (native-constant (native-value <long> 1)) (shape retval)))))

(build
  (sequence <int>)
  (address->scm
    (apply (asm ctx <ulong> (list <long> <long> <ulong>)
       (apply virtual-variables
         (assemble (list (delegate o)) (content (sequence <int>) s)
           (append (construct-value t (parameter s))
                   (code o (package-return-content t)))))) (unbuild (sequence <int>) (seq 2 3 5)))))


(run-tests)
