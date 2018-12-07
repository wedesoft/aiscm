(use-modules (oop goops) (ice-9 binary-ports) (rnrs bytevectors) (aiscm core) (system foreign) (aiscm xorg) (ice-9 format))
; http://yann.lecun.com/exdb/mnist/
(define f (open-file "train-labels-idx1-ubyte" "rb"))
(define magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define n (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define bv (get-bytevector-n f n))
(define labels (make (multiarray <ubyte> 1) #:memory (bytevector->pointer bv) #:shape (list n)))

(define f (open-file "train-images-idx3-ubyte" "rb"))
(define magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define n (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define h (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define w (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define bv (get-bytevector-n f (* n h w)))
(define images (make (multiarray <ubyte> 3) #:memory (bytevector->pointer bv) #:shape (list n h w)))

(define i -1)
(show
  (lambda _
    (set! i (modulo (1+ i) n))
    (format #t "~a~&" (get labels i))
    (- 255 (get images i)))
  #:io IO-OPENGL #:width 256)
