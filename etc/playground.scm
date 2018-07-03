(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(llvm-typed (list (multiarray <int> 2)) identity)

(define argument-types (list (multiarray <int> 2)))

(define function identity)

(append-map decompose-type argument-types)

(define result-type #f)

(define foreign-types (cons llvm-int64 (map foreign-type (append-map decompose-type argument-types))))

(define mod (make-llvm-module))

(define arguments (map function-param (iota (length foreign-types))))

(define arguments-typed (compose-values (cons <pointer<>> argument-types) arguments))
