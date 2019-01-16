(use-modules (oop goops) (aiscm core) (aiscm pulse) (aiscm tensorflow) (aiscm xorg))
(define s (make-session))
(define sample (tf-placeholder #:dtype <float> #:shape '(1 256)))
(define vec (tf-reshape sample (arr <int> 256)))
(define fourier (tf-rfft vec (arr <int> 256)))
(define spectrum (tf-real (tf-mul fourier (tf-conj fourier))))
(define record (make <pulse-record> #:typecode <float> #:channels 1 #:rate 11025))
(define ramp (/ (indices 256) 128.0))
(define window (to-type <float> (minor ramp (- 2.0 ramp))))
(define x 0)
(define img (fill <ubyte> '(129 256) 0))
(show
  (lambda _
    (let [(spec (run s (list (cons sample (* window (read-audio record 256)))) spectrum))]
      (set img x '(0 129) (minor 255 spec))
      (set! x (modulo (1+ x) 256))
      img)))
