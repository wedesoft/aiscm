(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(define m (make (multiarray <int> 1) #:shape '(32)))


((llvm-typed (list <byte> <byte> <int>)
  (lambda args
    (let [(p (map (lambda (arg) (if (is-a? arg <byte>) (list (typed-alloca (class-of arg))) '())) args))]
      (llvm-begin
        (apply llvm-begin (concatenate p))
        (apply llvm-begin (append-map (lambda (ptr arg) (if (null? ptr) '() (list (store (car ptr) arg)))) p args))
        (apply + (map fetch (concatenate p))))))) 2 3 5)
