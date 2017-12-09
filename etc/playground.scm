(use-modules (oop goops) (aiscm llvm) (system foreign) (rnrs bytevectors))

(define llvm (make <llvm>))

(define constant (make-function llvm double "constant_double2"))
(function-ret constant (make-constant double (exp 1)))

(define identity (make-function llvm int "identity" int))
(function-ret identity (function-param identity 0))

(define test (make-function llvm double "test" double))
(function-ret test (function-param test 0))

(llvm-verify llvm)
(llvm-dump llvm)

(llvm-apply llvm constant)
(llvm-apply llvm identity 12345)
(llvm-apply llvm test (exp 1))
