(use-modules (oop goops)
             (ice-9 binary-ports)
             (ice-9 format)
             (srfi srfi-1)
             (rnrs bytevectors)
             (system foreign)
             (aiscm core)
             (aiscm xorg)
             (aiscm tensorflow))

; Get MNIST data at http://yann.lecun.com/exdb/mnist/
(define f (open-file "train-images-idx3-ubyte" "rb"))
(define magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(if (not (eqv? magic 2051)) (error "Images file has wrong magic number"))
(define n (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define h (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define w (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(define bv (get-bytevector-n f (* n h w)))
(define images (make (multiarray <ubyte> 3) #:memory (bytevector->pointer bv) #:shape (list n h w)))

(define f (open-file "train-labels-idx1-ubyte" "rb"))
(define magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(if (not (eqv? magic 2049)) (error "Label file has wrong magic number"))
(define n2 (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
(if (not (eqv? n n2)) (error "Number of labels does not match number of images"))
(define bv (get-bytevector-n f n))
(define labels (make (multiarray <ubyte> 1) #:memory (bytevector->pointer bv) #:shape (list n)))

(define s (make-session))

(define x (tf-placeholder #:dtype <ubyte> #:shape '(-1 28 28)))
(define y (tf-placeholder #:dtype <ubyte> #:shape '(-1)))

(define r1 (tf-reshape (tf-sub (tf-mul (/ 1 256) (tf-cast x #:DstT <double>)) 0.5) (arr <int> -1 28 28 1)))
(define k1 (tf-variable #:dtype <double> #:shape '(3 3 1 4)))
(define c1 (tf-conv2d r1 k1 #:strides '(1 1 1 1) #:padding 'VALID))
(define p1 (tf-relu (tf-max-pool c1 #:strides '(1 2 2 1) #:ksize '(1 2 2 1) #:padding 'VALID)))

(define k2 (tf-variable #:dtype <double> #:shape '(3 3 4 16)))
(define c2 (tf-conv2d p1 k2 #:strides '(1 1 1 1) #:padding 'VALID))
(define p2 (tf-relu (tf-max-pool c2 #:strides '(1 2 2 1) #:ksize '(1 2 2 1) #:padding 'VALID)))

(define n (* 5 5 16))
(define r2 (tf-reshape p2 (to-array <int> (list -1 n))))
(define m1 (tf-variable #:dtype <double> #:shape (list n 40)))
(define b1 (tf-variable #:dtype <double> #:shape '(40)))
(define l1 (tf-relu (tf-add (tf-mat-mul r2 m1) b1)))

(define m2 (tf-variable #:dtype <double> #:shape '(40 10)))
(define b2 (tf-variable #:dtype <double> #:shape '(10)))
(define l (tf-softmax (tf-add (tf-mat-mul l1 m2) b2)))

(define prediction (tf-arg-max l 1 #:name "prediction"))

(run s '() (tf-assign k1 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 1 4) #:dtype <double>))))
(run s '() (tf-assign k2 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 4 16) #:dtype <double>))))
(run s '() (tf-assign m1 (tf-mul (/ 1 n) (tf-random-uniform (to-array <int> (list n 40)) #:dtype <double>))))
(run s '() (tf-assign b1 (fill <double> '(40) 0.0)))
(run s '() (tf-assign m2 (tf-mul (/ 1 40) (tf-random-uniform (arr <int> 40 10) #:dtype <double>))))
(run s '() (tf-assign b2 (fill <double> '(10) 0.0)))

(define vars (list k1 k2 m1 b1 m2 b2))

(define yh (tf-one-hot y 10 1.0 0.0))

(define cost (tf-neg (tf-mean (tf-add (tf-mul yh (tf-log l)) (tf-mul (tf-sub 1.0 yh) (tf-log (tf-sub 1.0 l)))) (arr <int> 0 1))))

(define gradients (tf-add-gradient cost vars))

(define alpha 0.1)
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

(for-each
  (lambda (epoch)
    (for-each
      (lambda (i)
        (let [(range (list i (+ i 50)))]
        (run s (list (cons x (unroll (get images range))) (cons y (get labels range))) step)))
      (iota (/ 60000 50) 0 50)))
  (iota 3))

(run s (list (cons x (get images '(0 . 20)))) prediction)
(get labels '(0 . 20))

;(define i -1)
;(show
;  (lambda _
;    (set! i (modulo (1+ i) n))
;    (format #t "~a~&" (get labels i))
;    (- 255 (get images i)))
;  #:io IO-OPENGL #:width 256)
