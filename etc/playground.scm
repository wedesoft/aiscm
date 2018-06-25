(use-modules (oop goops) (aiscm llvm) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm basictype) (srfi srfi-1) (srfi srfi-26))

(llvm-typed (list (pointer <byte>)) (lambda (x) (format #t "~a~&" x) x))
