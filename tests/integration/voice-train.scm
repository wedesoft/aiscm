(use-modules (oop goops)
             (ice-9 textual-ports)
             (ice-9 format)
             (srfi srfi-1)
             (aiscm core)
             (aiscm ffmpeg)
             (aiscm tensorflow)
             (aiscm util)
             (aiscm tensors))

(define words (list "stop" "go" "left" "right"))
(define n-hidden 64)
(define m 50)

(define x (tf-placeholder #:dtype <sint> #:shape '(-1 512) #:name "x"))
(define y (tf-placeholder #:dtype <ubyte> #:shape '(-1 4) #:name "y"))
(define h0 (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "h"))
(define c0 (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "c"))
(define wf (tf-variable #:dtype <double> #:shape (list 257 n-hidden)))
(define wi (tf-variable #:dtype <double> #:shape (list 257 n-hidden)))
(define wo (tf-variable #:dtype <double> #:shape (list 257 n-hidden)))
(define wc (tf-variable #:dtype <double> #:shape (list 257 n-hidden)))
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
  (list (tf-assign wf (tf-mul (/ 1 257) (tf-truncated-normal (to-array <int> (list 257 n-hidden)) #:dtype <double>)))
        (tf-assign wi (tf-mul (/ 1 257) (tf-truncated-normal (to-array <int> (list 257 n-hidden)) #:dtype <double>)))
        (tf-assign wo (tf-mul (/ 1 257) (tf-truncated-normal (to-array <int> (list 257 n-hidden)) #:dtype <double>)))
        (tf-assign wc (tf-mul (/ 1 257) (tf-truncated-normal (to-array <int> (list 257 n-hidden)) #:dtype <double>)))
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

(define (fourier x) (tf-reshape (tf-rfft (tf-cast x #:DstT <float>) (arr <int> 512)) (arr <int> 1 257)))

(define (spectrum x) (let [(f (fourier x))] (tf-log (tf-cast (tf-real (tf-mul f (tf-conj f))) #:DstT <double>))))

(define (lstm spectrum h c)
  (let* [(f (tf-sigmoid (tf-add-n (list (tf-mat-mul spectrum wf) (tf-mat-mul h uf) bf))))
         (i (tf-sigmoid (tf-add-n (list (tf-mat-mul spectrum wi) (tf-mat-mul h ui) bi))))
         (o (tf-sigmoid (tf-add-n (list (tf-mat-mul spectrum wo) (tf-mat-mul h uo) bo))))
         (g (tf-tanh (tf-add-n (list (tf-mat-mul spectrum wc) (tf-mat-mul h uc) bc))))
         (c_ (tf-add (tf-mul f c) (tf-mul i g)))
         (h_ (tf-mul o (tf-tanh c_)))]
    (list h_ c_)))

(define (output h) (tf-softmax (tf-add (tf-mat-mul h wy) by)))

(define (nth x i) (tf-nth-element (tf-transpose x (arr <int> 1 0)) i))

(define (safe-log x) (tf-log (tf-maximum x 1e-10)))

(define (invert x) (tf-sub 1.0 x))

(define h_ h0)
(define c_ c0)

(define loss 0.0)

(for-each
  (lambda (i)
    (let* [(memory (lstm (spectrum (nth x i)) h_ c_))
           (h      (output (car memory)))]
      (set! h_ (car memory))
      (set! c_ (cadr memory))
      (set! loss (tf-add loss (tf-mean (tf-add (tf-mul (tf-cast (nth y i) #:DstT <double>) (safe-log h))
                                               (tf-mul (invert (tf-cast (nth y i) #:DstT <double>)) (safe-log (invert h))))
                                       (arr <int> 0 1))))))
  (iota m))
(set! loss (tf-mul (tf-neg loss) (/ 1 m)))

(define s (make-session))

(define vars (list wf wi wo wc uf ui uo uc bf bi bo bc wy by))

(define gradients (tf-add-gradient loss vars))
(define alpha 0.5)
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

(define t 0.0)
(define word "stop")
(define next-t 0.0)
(define next-word "stop")

(define audio (open-ffmpeg-input "voice-commands.mp3"))
(define l (- m))
(while (read-audio audio (* m 512)) (set! l (+ l m)))

(define in (make (multiarray <sint> 2) #:shape (list l 512)))
(define out (make (multiarray <ubyte> 2) #:shape (list l 4)))

(define i 0)
(define audio (open-ffmpeg-input "voice-commands.mp3"))
(call-with-input-file "voice-commands.csv"
  (lambda (port)
    (get-line port)
    (while #t
      (let [(line (get-line port))
            (outer-break break)]
        (set! word next-word)
        (if (eof-object? line)
          (set! next-t 1e+99)
          (let [(columns (string-split line #\,))]
            (set! next-t (string->number (car columns)))
            (set! next-word (cadr columns))))
        (while (< t next-t)
          (let [(samples (read-audio audio 512))]
            (if (>= i l) (outer-break))
            (set in i (reshape (to-array samples) '(512)))
            (set out i 0)
            (set out (index-of word words) i 1)
            (set! i (1+ i))
            (set! t (+ t (/ 512 (rate audio))))))))))

(define session (make-session))

(run session '() initializers)

(define h #f)
(define c #f)
(define j 0.5)
(for-each
  (lambda (epoch)
    (set! h (zeros 1 n-hidden))
    (set! c (zeros 1 n-hidden))
    (for-each
      (lambda (i)
        (let* [(interval (cons i (+ i m)))
               (batch (list (cons h0 h) (cons c0 c) (cons x (unroll (get in interval))) (cons y (unroll (get out interval)))))
               (js (run session batch loss))]
          (set! j (+ (* 0.99 j) (* 0.01 js)))
          (format #t "\repoch ~2d, ~5d/~5d: ~6,4f" epoch i l j)
          (run session batch step)
          (set! h (run session batch h_))
          (set! c (run session batch c_))))
      (iota (/ l m) 0 m)))
  (iota 500))
(format #t "~&")
