(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define-class <testmixed> ()
              (test-a #:init-keyword #:test-a #:getter test-a)
              (test-b #:init-keyword #:test-b #:getter test-b))
(define (make-testmixed test-a test-b) (make <testmixed> #:test-a test-a #:test-b test-b))
(define-structure testmixed make-testmixed (test-a test-b))

(define-method (+ (a <testmixed<>>) (b <testmixed<>>)) (testmixed (+ (test-a a) (test-a b)) (+ (test-b a) (test-b b))))

(llvm-typed (list (testmixed <int> <sint>) (testmixed <int> <sint>)) +)
