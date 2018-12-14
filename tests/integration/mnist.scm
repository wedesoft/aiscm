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

(define (read-images file-name)
  (let* [(f     (open-file file-name "rb"))
         (magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
         (n     (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
         (h     (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
         (w     (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))]
    (if (not (eqv? magic 2051)) (error "Images file has wrong magic number"))
    (let [(bv (get-bytevector-n f (* n h w)))]
      (make (multiarray <ubyte> 3) #:memory (bytevector->pointer bv) #:shape (list n h w)))))

(define (read-labels file-name)
  (let* [(f     (open-file file-name "rb"))
         (magic (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))
         (n     (bytevector-u32-ref (get-bytevector-n f 4) 0 (endianness big)))]
    (if (not (eqv? magic 2049)) (error "Label file has wrong magic number"))
    (let [(bv (get-bytevector-n f n))]
      (make (multiarray <ubyte> 1) #:memory (bytevector->pointer bv) #:shape (list n)))))

(define images (read-images "train-images-idx3-ubyte"))
(define labels (read-labels "train-labels-idx1-ubyte"))
(define n (car (shape images)))

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

(define d (* 5 5 16))
(define r2 (tf-reshape p2 (to-array <int> (list -1 d))))
(define m1 (tf-variable #:dtype <double> #:shape (list d 40)))
(define b1 (tf-variable #:dtype <double> #:shape '(40)))
(define l1 (tf-relu (tf-add (tf-mat-mul r2 m1) b1)))

(define m2 (tf-variable #:dtype <double> #:shape '(40 10)))
(define b2 (tf-variable #:dtype <double> #:shape '(10)))
(define l (tf-softmax (tf-add (tf-mat-mul l1 m2) b2)))

(define prediction (tf-arg-max l 1 #:name "prediction"))

(run s '() (tf-assign k1 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 1 4) #:dtype <double>))))
(run s '() (tf-assign k2 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 4 16) #:dtype <double>))))
(run s '() (tf-assign m1 (tf-mul (/ 1 n) (tf-random-uniform (to-array <int> (list d 40)) #:dtype <double>))))
(run s '() (tf-assign b1 (fill <double> '(40) 0.0)))
(run s '() (tf-assign m2 (tf-mul (/ 1 40) (tf-random-uniform (arr <int> 40 10) #:dtype <double>))))
(run s '() (tf-assign b2 (fill <double> '(10) 0.0)))

(define vars (list k1 k2 m1 b1 m2 b2))

(define yh (tf-one-hot y 10 1.0 0.0))

(define penalty (tf-neg (tf-mean (tf-add (tf-mul yh (tf-log l)) (tf-mul (tf-sub 1.0 yh) (tf-log (tf-sub 1.0 l)))) (arr <int> 0 1))))

(define regularization (tf-add (tf-mean (tf-square m1) (arr <int> 0 1)) (tf-mean (tf-square m2) (arr <int> 0 1))))

(define la 0.0)
(define cost (tf-add penalty (tf-mul la regularization)))

(define gradients (tf-add-gradient cost vars))

(define alpha 0.4)
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

(define j 0.0)

(for-each
  (lambda (epoch)
    (for-each
      (lambda (i)
        (let* [(range (cons i (+ i 50)))
               (batch (list (cons x (unroll (get images range))) (cons y (get labels range))))
               (js    (run s batch cost))]
          (set! j (+ (* 0.99 j) (* 0.01 js)))
          (format #t "\r~2d, ~5d/~5d: ~6,4f" epoch i n j)
          (run s batch step)))
      (iota (/ n 50) 0 50)))
  (iota 3))
(format #t "~&")

(define test-images (read-images "t10k-images-idx3-ubyte"))
(define test-labels (read-labels "t10k-labels-idx1-ubyte"))
(define n-test (car (shape test-images)))

(define j (run s (list (cons x (unroll (get images '(0 . 10000)))) (cons y (get labels '(0 . 10000)))) penalty))
(define jt (run s (list (cons x test-images) (cons y test-labels)) penalty))
(format #t "train: ~6,4f; test: ~6,4f~&" j jt)

(define predicted (run s (list (cons x test-images)) prediction))
(define n-correct (sum (where (eq predicted test-labels) 1 0)))
(format #t "error rate: ~6,4f~&" (- 1.0 (/ n-correct n-test)))
