(use-modules (oop goops) (aiscm core) (aiscm pulse) (aiscm tensorflow) (aiscm xorg))
(define s (make-session))
(define sample (tf-placeholder #:dtype <float> #:shape '(1 512)))
(define vec (tf-reshape sample (arr <int> 512)))
(define fourier (tf-rfft vec (arr <int> 512)))
(define spectrum (tf-real (tf-mul fourier (tf-conj fourier))))
(define record (make <pulse-record> #:typecode <float> #:channels 1 #:rate 11025))
(define x 0)
(define img (fill <ubyte> '(257 512) 0))
(show
  (lambda _
    (let [(spec (run s (list (cons sample (read-audio record 512))) spectrum))]
      (set img x '(0 257) spec)
      (set! x (modulo (1+ x) 512))
      img)))
