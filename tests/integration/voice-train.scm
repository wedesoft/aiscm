(use-modules (oop goops)
             (ice-9 textual-ports)
             (ice-9 readline)
             (ice-9 format)
             (srfi srfi-1)
             (aiscm core)
             (aiscm ffmpeg)
             (aiscm tensorflow)
             (aiscm util)
             (aiscm tensors))

(define words (list "stop" "go" "left" "right"))
(define chunk 512)
(define chunk2 (1+ (/ chunk 2)))
(define n-hidden 64)
(define m 10)
(define input (open-ffmpeg-input "voice-commands.mp3"))
(define csv (open-file "voice-commands.csv" "r"))
(get-line csv)
(define features '())
(define labels '())
(while #t
  (let [(line (get-line csv))]
    (if (eof-object? line) (break))
    (let* [(row     (string-split line #\,))
           (l       (string->number (car row)))
           (word    (cadr row))
           (samples (reshape (to-array (read-audio input (* chunk l))) (list l chunk)))]
      (set! features (attach features samples))
      (set! labels (attach labels (index-of word words))))))

(define x (tf-placeholder #:dtype <sint> #:shape (list -1 chunk) #:name "x"))
(define y (tf-placeholder #:dtype <int> #:shape '(-1) #:name "y"))
(define yh (tf-one-hot (tf-cast y #:DstT <int>) (length words) 1.0 0.0))
(define (nth x i) (tf-nth-element (tf-transpose x (arr <int> 1 0)) i))
(define (fourier x) (tf-reshape (tf-rfft (tf-cast x #:DstT <float>) (to-array <int> (list chunk))) (arr <int> 1 -1)))
(define (spectrum x) (let [(f (fourier x))] (tf-log (tf-cast (tf-real (tf-mul f (tf-conj f))) #:DstT <double>))))
(define h (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "h"))
(define c (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "c"))

(define wf (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define wi (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define wo (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define wc (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define uf (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define ui (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define uo (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define uc (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define bf (tf-variable #:dtype <double> #:shape (list 1 n-hidden)))
(define bi (tf-variable #:dtype <double> #:shape (list 1 n-hidden)))
(define bo (tf-variable #:dtype <double> #:shape (list 1 n-hidden)))
(define bc (tf-variable #:dtype <double> #:shape (list 1 n-hidden)))
(define wy (tf-variable #:dtype <double> #:shape (list n-hidden 4)))
(define by (tf-variable #:dtype <double> #:shape (list 1 4)))

(define (zeros . shape) (fill <double> shape 0.0))

(define initializers
  (list (tf-assign wf (tf-mul (/ 1 chunk2) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
        (tf-assign wi (tf-mul (/ 1 chunk2) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
        (tf-assign wo (tf-mul (/ 1 chunk2) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
        (tf-assign wc (tf-mul (/ 1 chunk2) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
        (tf-assign uf (tf-mul (/ 1 n-hidden) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
        (tf-assign ui (tf-mul (/ 1 n-hidden) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
        (tf-assign uo (tf-mul (/ 1 n-hidden) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
        (tf-assign uc (tf-mul (/ 1 n-hidden) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
        (tf-assign bf (zeros 1 n-hidden))
        (tf-assign bi (zeros 1 n-hidden))
        (tf-assign bo (zeros 1 n-hidden))
        (tf-assign bc (zeros 1 n-hidden))
        (tf-assign wy (tf-mul (/ 1 n-hidden) (tf-truncated-normal (to-array <int> (list n-hidden 4)) #:dtype <double>)))
        (tf-assign by (fill <double> '(1 4) 0.0))))

(define vars (list wf wi wo wc uf ui uo uc bf bi bo bc wy by))

(define (lstm x h c)
  (let* [(f (tf-sigmoid (tf-add-n (list (tf-mat-mul x wf) (tf-mat-mul h uf) bf))))
         (i (tf-sigmoid (tf-add-n (list (tf-mat-mul x wi) (tf-mat-mul h ui) bi))))
         (o (tf-sigmoid (tf-add-n (list (tf-mat-mul x wo) (tf-mat-mul h uo) bo))))
         (g (tf-tanh (tf-add-n (list (tf-mat-mul x wc) (tf-mat-mul h uc) bc))))
         (c_ (tf-add (tf-mul f c) (tf-mul i g)))
         (h_ (tf-mul o (tf-tanh c_)))]
    (cons h_ c_)))

(define (output x) (tf-sigmoid (tf-add (tf-mat-mul x wy) by)))

(define (safe-log x) (tf-log (tf-maximum x 1e-10)))

(define (invert x) (tf-sub 1.0 x))

(define h_ h)
(define c_ c)

(define alpha 0.001)

(define losses '())
(define steps '())

(for-each
  (lambda (i)
    (let* [(memory    (lstm (spectrum (nth x i)) h_ c_))
           (y_        (output (car memory)))
           (loss      (tf-neg (tf-mean (tf-add (tf-mul yh (safe-log y_))
                                               (tf-mul (invert yh) (safe-log (invert y_))))
                                       (arr <int> 0 1))))
           (gradients (tf-add-gradient loss vars))
           (step      (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))]
      (set! losses (attach losses loss))
      (set! steps (attach steps step))
      (set! h_ (car memory))
      (set! c_ (cdr memory))))
  (iota m))

(define h0 (zeros 1 n-hidden))
(define c0 (zeros 1 n-hidden))

(define session (make-session))

(run session '() initializers)

(define j 0.680)

(for-each
  (lambda (epoch)
    (for-each
      (lambda (feature label)
        (let* [(batch (list (cons h h0) (cons c c0) (cons x feature) (cons y label)))
               (l     (car (shape feature)))
               (js    (run session batch (list-ref losses (1- l))))]
          (set! j (+ (* 0.999 j) (* 0.001 js)))
          (format #t "epoch ~2d: ~6,4f (~6,4f)\r" epoch j js)
          (run session batch (list-ref steps (1- l)))))
      features labels))
  (iota 1000))
