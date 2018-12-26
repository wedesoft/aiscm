(use-modules (oop goops) (ice-9 textual-ports) (ice-9 format) (aiscm core) (aiscm ffmpeg) (aiscm tensorflow) (aiscm util))

(define words (list "stop" "go" "left" "right"))
(define n-hidden 64)
(define m 20)

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

(define (fourier x)
  (tf-cast (tf-reshape (tf-rfft (tf-cast x #:DstT <float>) (arr <int> 512)) (arr <int> 1 257)) #:DstT <double>))

(define (lstm fourier h c)
  (let* [(f (tf-add-n (list (tf-mat-mul fourier wf) (tf-mat-mul h uf) bf)))
         (i (tf-add-n (list (tf-mat-mul fourier wi) (tf-mat-mul h ui) bi)))
         (o (tf-add-n (list (tf-mat-mul fourier wo) (tf-mat-mul h uo) bo)))
         (g (tf-tanh (tf-add-n (list (tf-mat-mul fourier wc) (tf-mat-mul h uc) bc))))
         (c_ (tf-add (tf-mul f c) (tf-mul i g)))
         (h_ (tf-mul o (tf-tanh c_)))]
    (list h_ c_)))

(define (output h) (tf-add (tf-mat-mul h wy) by))

(define (nth x i) (tf-nth-element (tf-transpose x (arr <int> 1 0)) i))

(define (invert x) (tf-sub 1.0 x))

(define h_ h0)
(define c_ c0)

(define cost 0.0)

(for-each
  (lambda (i)
    (let* [(memory (lstm (fourier (nth x i)) h_ c_))
           (h      (output (car memory)))]
      (set! h_ (car memory))
      (set! c_ (cadr memory))
      (set! cost (tf-add cost (tf-mean (tf-add (tf-mul (tf-cast (nth y i) #:DstT <double>) (tf-log h))
                                               (tf-mul (invert (tf-cast (nth y i) #:DstT <double>)) (tf-log (invert h))))
                                       (arr <int> 0))))))
  (iota m))

(define s (make-session))

(define vars (list wf wi wo wc uf ui uo uc bf bi bo bc wy by))

(define gradient (tf-add-gradient cost vars))

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
