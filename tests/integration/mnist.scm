(use-modules (oop goops)
             (ice-9 binary-ports)
             (ice-9 format)
             (srfi srfi-1)
             (rnrs bytevectors)
             (system foreign)
             (aiscm core)
             (aiscm xorg)
             (aiscm util)
             (aiscm tensorflow))

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

; Load MNIST data set available at http://yann.lecun.com/exdb/mnist/
(define images (read-images "train-images-idx3-ubyte"))
(define labels (read-labels "train-labels-idx1-ubyte"))
(define n (car (shape images)))

; Create Tensorflow session
(define s (make-session))

; Define placeholders for features (images) and labels
(define x (tf-placeholder #:dtype <ubyte> #:shape '(-1 28 28)))
(define y (tf-placeholder #:dtype <ubyte> #:shape '(-1)))

; Determine parameters for feature scaling
(define (sqr x) (* x x))
(define count (apply * (shape images)))
(define average (exact->inexact (/ (sum images) count)))
(define stddev (sqrt (/ (sum (sqr (- images average))) count)))

; Scale features and reshape to 4D array
(define r1 (tf-reshape (tf-mul (/ 1 stddev) (tf-sub (tf-cast x #:DstT <double>) average)) (arr <int> -1 28 28 1)))

; First convolutional layer with max-pooling and ReLU activation function.
(define k1 (tf-variable #:dtype <double> #:shape '(3 3 1 4)))
(define c1 (tf-conv2d r1 k1 #:strides '(1 1 1 1) #:padding 'VALID))
(define p1 (tf-relu (tf-max-pool c1 #:strides '(1 2 2 1) #:ksize '(1 2 2 1) #:padding 'VALID)))

; Second convolutional layer with max-pooling and ReLU activation function.
(define k2 (tf-variable #:dtype <double> #:shape '(3 3 4 16)))
(define c2 (tf-conv2d p1 k2 #:strides '(1 1 1 1) #:padding 'VALID))
(define p2 (tf-relu (tf-max-pool c2 #:strides '(1 2 2 1) #:ksize '(1 2 2 1) #:padding 'VALID)))

; Reshape to 2D array
(define d (* 5 5 16))
(define r2 (tf-reshape p2 (to-array <int> (list -1 d))))

; First fully connected layer with bias units and ReLU activation function.
(define m1 (tf-variable #:dtype <double> #:shape (list d 40)))
(define b1 (tf-variable #:dtype <double> #:shape '(40)))
(define l1 (tf-relu (tf-add (tf-mat-mul r2 m1) b1)))

; Second fully connected layer with bias units and softmax activation function.
(define m2 (tf-variable #:dtype <double> #:shape '(40 10)))
(define b2 (tf-variable #:dtype <double> #:shape '(10)))
(define l (tf-softmax (tf-add (tf-mat-mul l1 m2) b2)))

; Classification result of neural network.
(define prediction (tf-arg-max l 1 #:name "prediction"))

; Random initialization of network parameters
(run s '() (tf-assign k1 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 1 4) #:dtype <double>))))
(run s '() (tf-assign k2 (tf-mul (/ 1 9) (tf-random-uniform (arr <int> 3 3 4 16) #:dtype <double>))))
(run s '() (tf-assign m1 (tf-mul (/ 1 n) (tf-random-uniform (to-array <int> (list d 40)) #:dtype <double>))))
(run s '() (tf-assign b1 (fill <double> '(40) 0.0)))
(run s '() (tf-assign m2 (tf-mul (/ 1 40) (tf-random-uniform (arr <int> 40 10) #:dtype <double>))))
(run s '() (tf-assign b2 (fill <double> '(10) 0.0)))

; List of all network parameters
(define vars (list k1 k2 m1 b1 m2 b2))

; Logistic loss function
(define yh (tf-one-hot y 10 1.0 0.0))
(define (safe-log x) (tf-log (tf-maximum x 1e-10)))
(define loss (tf-neg (tf-mean (tf-add (tf-mul yh (safe-log l)) (tf-mul (tf-sub 1.0 yh) (safe-log (tf-sub 1.0 l)))) (arr <int> 0 1))))

; Regularization term
(define regularization (tf-add (tf-mean (tf-square (tf-abs m1)) (arr <int> 0 1)) (tf-mean (tf-square (tf-abs m2)) (arr <int> 0 1))))

; Overall cost function
(define la 0.02)
(define cost (tf-add loss (tf-mul la regularization)))

; Implement gradient descent step
(define gradients (tf-add-gradient cost vars))
(define alpha 0.4)
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

; Perform gradient descent
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

; Load MNIST test data.
(define test-images (read-images "t10k-images-idx3-ubyte"))
(define test-labels (read-labels "t10k-labels-idx1-ubyte"))
(define n-test (car (shape test-images)))

; Display cost function result for (part of) training and test data
(define j (run s (list (cons x (unroll (get images '(0 . 10000)))) (cons y (get labels '(0 . 10000)))) loss))
(define jt (run s (list (cons x test-images) (cons y test-labels)) loss))
(format #t "train: ~6,4f; test: ~6,4f~&" j jt)

; Determine error rate
(define predicted (run s (list (cons x test-images)) prediction))
(define n-correct (sum (where (eq predicted test-labels) 1 0)))
(format #t "error rate: ~6,4f~&" (- 1.0 (/ n-correct n-test)))

; Display individual results
(define time (clock))
(define i -1)
(show
  (lambda (dsp)
    (set! i (1+ i))
    (let* [(image (get test-images (modulo i 10000)))
           (pred (get (run s (list (cons x image)) prediction) 0))]
      (synchronise image (- i (elapsed time)) (event-loop dsp))
      (format #t "~a~&" pred)
      image))
  #:width 280)
