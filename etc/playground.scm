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
             (aiscm complex)
             (aiscm obj)
             (aiscm asm)
             (aiscm jit)
             (aiscm method)
             (aiscm util))

(define-syntax-rule (d expr) (format #t "~a = ~a~&" (quote expr) expr))

(define ctx (make <context>))

(define v (seq 1 2 3))

(define s (parameter (sequence <ubyte>)))
(define t (parameter (sequence <ubyte>)))
(define f (+ s t))

(d s)
(d (index s))
(d (class-of (index s)))
(d (typecode (index s)))

(d (delegate s))
(d (index (delegate s)))
(d (stride (delegate s)))
(d (step (delegate s)))
(d (iterator (delegate s)))
(d (delegate (delegate s)))
(d (delegate (delegate (delegate s))))

(d (iterator s))

(define-method (iterators (self <param>)) (list (iterator self)))
(define-method (iterators (self <function>)) (append-map iterators (arguments self)))
(define-method (steps (self <param>)) (list (step self)))
(define-method (steps (self <function>)) (append-map steps (arguments self)))
(define-method (strides (self <param>)) (list (stride self)))
(define-method (strides (self <function>)) (append-map strides (arguments self)))

(d (iterators f))
(d (steps f))
(d (strides f))

(d (setup s))
