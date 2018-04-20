(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define-class <vec> ()
              (x #:init-keyword #:x #:getter x)
              (y #:init-keyword #:y #:getter y)
              (z #:init-keyword #:z #:getter z))
(define-method (write (self <vec>) port) (format port "[~a ~a ~a]" (x self) (y self) (z self)))
(define (make-vec x y z) (make <vec> #:x x #:y y #:z z))
(define-structure vec make-vec (x y z))
(define-uniform-constructor vec)

((llvm-typed (list (vec <float>)) identity) (make-vec 2 3 5))

(define-method (+ (a <vec<>>) (b <vec<>>)) (vec (+ (x a) (x b)) (+ (y a) (y b)) (+ (z a) (z b))))

((llvm-typed (list (vec <float>) (vec <float>)) +) (make-vec 2 3 5) (make-vec 3 5 7))

(define-class <state> ()
              (v #:init-keyword #:v #:getter v))
(define-method (write (self <state>) port) (format port "state(~a)" (v self)))
(define (make-state v) (make <state> #:v v))
(define-structure state make-state (x y z))
(define-mixed-constructor state)

(make-state (make-vec 2 3 5))

;(llvm-typed (list (state (vec <float>))) identity)
(define argument-types (list (state (vec <float>))))
(define function identity)

(map class-name (decompose-types argument-types))
; compose-values
