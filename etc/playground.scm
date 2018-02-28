(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(define-macro (testing name args type fun)
  (let [(header (map list args (make-list (length args) type)))]
    `(define-method (,name ,@header) (,fun ,@args))))

(define-syntax testing
  (lambda  (x)
    (syntax-case x ()
      ((k (name type args ...) body ...)
       (let [(header (map (lambda (arg) (list arg (syntax->datum #'type))) (syntax->datum #'(args ...))))]
         #`(define-method (name #,@(datum->syntax #'k header)) body ...))))))

(testing (t <real> a b) +)
