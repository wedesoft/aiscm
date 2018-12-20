(use-modules (oop goops) (ice-9 textual-ports) (ice-9 format) (aiscm core) (aiscm ffmpeg) (aiscm tensorflow))

(define audio (open-ffmpeg-input "voice-commands.mp3"))
(define words (list "stop" "go" "left" "right"))
(define t 0.0)
(define word "stop")
(define next-t 0.0)
(define next-word "stop")
(define n-hidden 64)

(define x (tf-placeholder #:dtype <sint> #:shape '(1 512) #:name "x"))
(define h (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "h"))
(define c (tf-placeholder #:dtype <double> #:shape (list 1 n-hidden) #:name "c"))
(define fourier (tf-cast (tf-rfft (tf-cast x #:DstT <float>) (arr <int> 512)) #:DstT <double>))
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

(define (lstm fourier h c)
  (let* [(f (tf-add-n (list (tf-mat-mul fourier wf) (tf-mat-mul h uf) bf)))
         (i (tf-add-n (list (tf-mat-mul fourier wi) (tf-mat-mul h ui) bi)))
         (o (tf-add-n (list (tf-mat-mul fourier wo) (tf-mat-mul h uo) bo)))
         (g (tf-tanh (tf-add-n (list (tf-mat-mul fourier wc) (tf-mat-mul h uc) bc))))
         (c_ (tf-add (tf-mul f c) (tf-mul i g)))
         (h_ (tf-mul o (tf-tanh c_)))]
    (list h_ c_)))

(lstm fourier h c)

(call-with-input-file "voice-commands.csv"
  (lambda (port)
    (get-line port)
    (while #t
      (let [(line (get-line port))]
        (if (eof-object? line) (break))
        (let [(columns (string-split line #\,))]
          (set! word next-word)
          (set! next-t (string->number (car columns)))
          (set! next-word (cadr columns))))
      (while (< t next-t)
        (let [(samples (read-audio audio 512))]
          (if (or (not samples) (< (car (shape samples)) 512))
            (break))
          (format #t "current output: ~a ~a~&" word (typecode samples))
          (set! t (+ t (/ 512 (rate audio)))))))))
