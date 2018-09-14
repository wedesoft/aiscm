(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(define argument-types (list <obj>))
(define function identity)

(define foreign-types (list 10 10))

(define mod (make-llvm-module))
(define arguments (map function-param (iota 2)))
(define result (apply function arguments))

(define arguments-typed (compose-values (cons <pointer<>> argument-types) arguments))
(define expression (apply function (cdr arguments-typed)))
(define result-type (class-of expression))
