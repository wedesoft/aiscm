(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

;(llvm-typed (list (multiarray <int> 2)) identity)

(define argument-types (list (multiarray <int> 2)))

(define function identity)

(append-map decompose-type argument-types)

(define result-type #f)

(define foreign-types (cons llvm-int64 (map foreign-type (append-map decompose-type argument-types))))

(define mod (make-llvm-module))

(define arguments (map function-param (iota (length foreign-types))))

(define arguments-typed (compose-values (cons <pointer<>> argument-types) arguments))

(define expression (apply function (cdr arguments-typed)))

(set! result-type (class-of expression))

(define result expression)

(define memory_ (car arguments-typed))

;(store (to-type (pointer (class-of result)) memory_) result)
(define ptr (to-type (pointer (class-of result)) memory_))
(define value result)

(define type (target (class-of ptr)))

(decompose-argument type value)
