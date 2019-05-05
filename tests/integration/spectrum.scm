(use-modules (oop goops) (aiscm core) (aiscm pulse) (aiscm tensorflow) (aiscm xorg))
(define s (make-session))
(define w 400)
(define h 512)
(define sample (tf-placeholder #:dtype <float> #:shape (list 1 h)))
(define vec (tf-reshape sample (to-array <int> (list h))))
(define fourier (tf-rfft vec (to-array <int> (list h))))
(define spectrum (tf-mul (tf-log (tf-add (tf-real (tf-mul fourier (tf-conj fourier))) (tf-cast 1.0 #:DstT <float>))) (tf-cast 32.0 #:DstT <float>)))
(define record (make <pulse-record> #:typecode <float> #:channels 1 #:rate 11025))
(define ramp (* (indices h) (/ 2.0 h)))
(define window (to-type <float> (minor ramp (- 2.0 ramp))))
(define x 0)
(define img (fill <ubyte> (list (1+ (/ h 2)) w) 0))
(show
  (lambda _
    (let [(spec (run s (list (cons sample (* window (read-audio record h)))) spectrum))]
      (set img x (list 0 (1+ (/ h 2))) (major (minor 255 spec) 0))
      (set! x (modulo (1+ x) w))
      img)))
