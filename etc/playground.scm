(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm convolution) (aiscm sequence) (aiscm operation) (aiscm expression) (aiscm loop) (srfi srfi-1) (aiscm command) (aiscm int) (aiscm variable) (aiscm asm) (aiscm rgb) (aiscm int) (aiscm jit) (aiscm scalar) (aiscm element) (ice-9 curried-definitions) (aiscm util) (aiscm compile))

(define a (var <int>))
(define x (var <int>))
(define p (var <long>))

; have temporary-variables return varying list of temporaries

(replace-variables (list (cons x (ptr <int> RSP 8)) (cons p (ptr <long> RSP 16))) (MOV x (ptr <int> p)) RAX)

(replace-variables (list (cons p (ptr <long> RSP 8)) (cons x (ptr <int> RSP 16))) (MOV (ptr <int> p) x) RAX)

(objdump (list (MOV EAX (ptr <int> RCX))))

(test-begin "playground")
(test-end "playground")
