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

; TODO: strides of parameter should be a list of parameters
; TODO: shape of function

(define s (skeleton (sequence <ubyte>)))
(define t (parameter s))

(define o (parameter <obj>))

(define u (parameter (sequence <ubyte>)))

;(make (sequence <ubyte>) #:shape (shape s) #:strides (strides s) #:value (value s))

(define (to-para x) (parameter (make <long> #:value x)))

; TODO: create value and initialisation code
(define (construct-value retval expr)
  (append (append-map code (shape retval) (shape expr))
          (code (car (content (pointer <ubyte>) (project retval)))
                (native-call scm-gc-malloc-pointerless (car (shape retval))))
          (code (to-para (stride expr)) (native-constant (native-value <int> 1)))))

(build
  (sequence <ubyte>)
  (address->scm
    (apply (asm ctx <ulong> (list <long> <long> <ulong>)
       (apply virtual-variables
         (assemble (list (delegate o)) (content (sequence <ubyte>) s)
           (append (construct-value t (parameter s))
                   (code o (package-return-content t)))))) (unbuild (sequence <ubyte>) (seq 2 3 5)))))


((jit ctx (list <int> <int>) build-list) 2 3)

(build (sequence <ubyte>)
 ((jit ctx (list <long>)
   (lambda (n) (build-list n (native-constant (native-value <int> 1)) (native-call scm-gc-malloc-pointerless n)))) 3))

(run-tests)
