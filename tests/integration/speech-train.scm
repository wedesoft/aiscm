(use-modules (oop goops) (ice-9 ftw) (ice-9 regex) (srfi srfi-1) (srfi srfi-26) (aiscm core) (aiscm ffmpeg)
             (aiscm samples) (aiscm tensorflow) (aiscm util))

(define words (list "stop" "go" "left" "right"))
(define rate 11025)
(define factor (/ 1.0 32768))
(define chunk 512); 21.5 chunks per second
(define max-delay 60); maximum number of chunks between two spoken words
(define signal 10); number of chunks where signal is kept on
(define chunk2 (1+ (/ chunk 2)))
(define file-names (filter (cut string-match "speech-.*\\.wav" <>) (scandir ".")))
(define n-hidden 128)
(define seconds 300); background noise seconds
(define count (ceiling (/ (* rate seconds) chunk)))
(define window 100)

(define background (reshape (from-samples (read-audio (open-ffmpeg-input "background.wav") (* count chunk))) (list count chunk)))

(define data
  (map
    (lambda (file-name)
      (let* [(match (string-match "speech-(.*)-(.*)-(.*)\\.wav" file-name))
             (word  (match:substring match 2))
             (index (list-index (cut equal? word <>) words))
             (input (open-ffmpeg-input file-name))
             (count (string->number (match:substring match 3)))
             (n     (/ count chunk)) ]
        (cons index (reshape (from-samples (read-audio input count)) (list n chunk)))))
    file-names))

(define (shift background)
  (let* [(len    (car (shape background)))
         (offset (random len))
         (result (make (multiarray <sint> 2) #:shape (shape background)))]
    (set result (cons 0 offset) (get background (cons (- len offset) len)))
    (set result (cons offset len) (get background (cons 0 (- len offset))))
    result))

(define (create-sample offset)
  (let* [(pause     (random max-delay))
         (idx       (random (length data)))
         (item      (list-ref data idx))
         (label     (car item))
         (candidate (cdr item))
         (len       (car (shape candidate)))]
    (if (>= (+ offset pause len signal) count)
      (cons (shift background)
            (fill <int> (list (car (shape background))) 0))
      (let [(sample   (create-sample (+ offset pause len signal)))
            (interval (cons (+ offset pause) (+ offset pause len)))]
        (set (car sample) (cons 0 chunk) interval (+ candidate (get background (cons 0 chunk) interval)))
        (set (cdr sample) (cons (+ offset pause len) (+ offset pause len signal)) (1+ label))
        sample))))

(define x (tf-placeholder #:dtype <sint> #:shape (list -1 chunk) #:name "x"))
(define y (tf-placeholder #:dtype <int> #:shape '(-1) #:name "y"))
(define c (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "c"))
(define y-hot (tf-one-hot y (1+ (length words)) 1.0 0.0))

(define (fourier x) (tf-rfft (tf-mul (tf-cast x #:DstT <float>) (tf-cast factor #:DstT <float>)) (to-array <int> (list chunk))))
(define (spectrum x) (let [(f (fourier x))] (tf-log (tf-add (tf-cast (tf-real (tf-mul f (tf-conj f))) #:DstT <double>) 1.0))))
(define (nth x i) (tf-expand-dims (tf-gather x i) 0))

(define wcc (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define wcx (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define bc  (tf-variable #:dtype <double> #:shape (list n-hidden)))
(define wuc (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define wux (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define bu  (tf-variable #:dtype <double> #:shape (list n-hidden)))
(define w   (tf-variable #:dtype <double> #:shape (list n-hidden 5)))
(define b   (tf-variable #:dtype <double> #:shape (list 5)))

(define vars (list wcc wcx bc wuc wux bu w b))

(define initializers
  (list
    (tf-assign wcc (tf-mul (sqrt (/ 2 n-hidden)) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
    (tf-assign wcx (tf-mul (sqrt (/ 2 chunk2)) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
    (tf-assign bc  (fill <double> (list n-hidden) 0.0))
    (tf-assign wuc (tf-mul (sqrt (/ 2 n-hidden)) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
    (tf-assign wux (tf-mul (sqrt (/ 2 chunk2)) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
    (tf-assign bu  (fill <double> (list n-hidden) 0.0))
    (tf-assign w   (tf-mul (sqrt (/ 2 n-hidden)) (tf-truncated-normal (to-array <int> (list n-hidden 5)) #:dtype <double>)))
    (tf-assign b   (fill <double> (list 5) 0.0))))

(define (gru x c)
  (let* [(gu (tf-sigmoid (tf-add (tf-add (tf-mat-mul c wuc) (tf-mat-mul x wux)) bu)))
         (cs (tf-tanh (tf-add (tf-add (tf-mat-mul (tf-mul gu c) wcc) (tf-mat-mul x wcx)) bc)))]
    (tf-add (tf-mul gu c) (tf-mul (tf-sub 1.0 gu) cs))))
(define (output c)
  (tf-softmax (tf-add (tf-mat-mul c w) b)))

(define c_ c)
(define outputs '())
(for-each
  (lambda (i)
    (set! c_ (gru (spectrum (nth x i)) c_))
    (set! outputs (attach outputs (output c_))))
  (iota window))

(define cs (tf-identity (gru (spectrum x) c) #:name "cs"))
(define pred (tf-gather (tf-arg-max (output c) 1) 0 #:name "prediction"))

(define (safe-log x) (tf-log (tf-maximum x 1e-10)))
(define loss (tf-div (tf-neg (tf-add-n (map (lambda (output i) (tf-sum (tf-mul (safe-log output) (nth y-hot i))
                                                                       (arr <int> 0 1)))
                                            outputs
                                            (iota window))))
                     (tf-cast window #:DstT <double>)))

(define alpha 0.05)
(define gradients (tf-add-gradient loss vars))
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

(define session (make-session))

(run session '() initializers)

(define j 1.0)

(for-each
  (lambda (epoch)
    (let [(sample (create-sample 0))
          (c0 (fill <double> (list 1 n-hidden) 0.0))]
      (for-each
        (lambda (i)
          (let* [(range (cons (* i window) (* (1+ i) window)))
                 (batch (list (cons x (get (car sample) (cons 0 chunk) range)) (cons y (get (cdr sample) range))))
                 (l     (run session (cons (cons c c0) batch) loss))]
            (set! j (+ (* 0.99 j) (* 0.01 l)))
            (run session (cons (cons c c0) batch) step)
            (format #t "~a ~a~&" epoch j)
            (set! c0 (run session (cons (cons c c0) batch) c_))))
        (iota (floor (/ count window))))))
  (iota 200))

(tf-assign wcc (tf-const #:value (run session '() wcc) #:dtype <double>) #:name "init-wcc")
(tf-assign wcx (tf-const #:value (run session '() wcx) #:dtype <double>) #:name "init-wcx")
(tf-assign bc  (tf-const #:value (run session '() bc ) #:dtype <double>) #:name "init-bc" )
(tf-assign wuc (tf-const #:value (run session '() wuc) #:dtype <double>) #:name "init-wuc")
(tf-assign wux (tf-const #:value (run session '() wux) #:dtype <double>) #:name "init-wux")
(tf-assign bu  (tf-const #:value (run session '() bu ) #:dtype <double>) #:name "init-bu" )
(tf-assign w   (tf-const #:value (run session '() w  ) #:dtype <double>) #:name "init-w"  )
(tf-assign b   (tf-const #:value (run session '() b  ) #:dtype <double>) #:name "init-b"  )

(tf-graph-export "speech-model.meta")
