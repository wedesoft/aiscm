(use-modules (ice-9 ftw) (ice-9 regex) (srfi srfi-26) (aiscm core) (aiscm ffmpeg) (aiscm tensorflow))

(define words (list "stop" "go" "left" "right"))
(define rate 11025)
(define chunk 512); 21.5 chunks per second
(define chunk2 (1+ (/ chunk 2)))
(define file-names (filter (cut string-match "speech-.*\\.mp3" <>) (scandir ".")))
(define n-hidden 16)

(define data
  (map
    (lambda (file-name)
      (let* [(match (string-match "speech-(.*)-(.*)-(.*)\\.mp3" file-name))
             (word  (match:substring match 2))
             (input (open-ffmpeg-input file-name))
             (count (string->number (match:substring match 3)))
             (n     (/ count chunk)) ]
        (list word (reshape (to-array (read-audio input count)) (list n chunk)))))
    file-names))

(define x (tf-placeholder #:dtype <sint> #:shape (list -1 chunk) #:name "x"))
(define y (tf-placeholder #:dtype <int> #:shape '(-1) #:name "y"))
(define c (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "c"))
(define y-hot (tf-one-hot y (1+ (length words)) 1.0 0.0))
(define (fourier x) (tf-rfft (tf-cast x #:DstT <float>) (to-array <int> (list chunk))))
(define (spectrum x) (let [(f (fourier x))] (tf-log (tf-add (tf-cast (tf-real (tf-mul f (tf-conj f))) #:DstT <double>) 1.0))))

(define wcc (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define wcx (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define bc (tf-variable #:dtype <double> #:shape (list n-hidden)))
(define wuc (tf-variable #:dtype <double> #:shape (list n-hidden n-hidden)))
(define wux (tf-variable #:dtype <double> #:shape (list chunk2 n-hidden)))
(define bu (tf-variable #:dtype <double> #:shape (list n-hidden)))

(define initializers
  (list
    (tf-assign wcc (tf-mul (sqrt (/ 2 n-hidden)) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
    (tf-assign wcx (tf-mul (sqrt (/ 2 chunk2)) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
    (tf-assign bc (fill <double> (list n-hidden) 0.0))
    (tf-assign wuc (tf-mul (sqrt (/ 2 n-hidden)) (tf-truncated-normal (to-array <int> (list n-hidden n-hidden)) #:dtype <double>)))
    (tf-assign wux (tf-mul (sqrt (/ 2 chunk2)) (tf-truncated-normal (to-array <int> (list chunk2 n-hidden)) #:dtype <double>)))
    (tf-assign bu (fill <double> (list n-hidden) 0.0))))

(define (gru x c)
  (let* [(cs (tf-tanh (tf-add (tf-add (tf-mat-mul c wcc) (tf-mat-mul x wcx)) bc)))
         (gu (tf-sigmoid (tf-add (tf-add (tf-mat-mul c wuc) (tf-mat-mul x wux)) bu)))]
    (tf-add (tf-mul gu cs) (tf-mul (tf-sub 1.0 gu) c))))

(define c0 (fill <double> (list 1 n-hidden) 0.0))

(define session (make-session))

(run session '() initializers)

(run session (list (cons c c0) (cons x (cadar data))) (gru (spectrum x) c))
