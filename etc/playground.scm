(use-modules (oop goops)
             (srfi srfi-1)
             (aiscm sequence)
             (aiscm expression)
             (aiscm asm)
             (aiscm element)
             (aiscm int)
             (aiscm jit)
             (aiscm util))

(test-begin "playground")

(define ctx (make <context>))

(define m (skeleton (sequence <int>)))
(define c (skeleton <int>))

(define mp (parameter m))
(define cp (parameter c))

(define prog (virtual-variables '() (content-vars (list m c)) (attach (code mp cp) (RET))))

(define (content-vars args) (map get (append-map content (map class-of args) args)))

(define instructions (asm ctx <null> (map typecode (content-vars (list m c))) prog))

(define (fun a b) (apply instructions (append-map unbuild (map class-of (list m c)) (list a b))))

(define mm (seq <int> 2 3 5))
(define cc 123)

(fun mm cc)

(test-end "playground")
