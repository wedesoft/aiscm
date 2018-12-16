(use-modules (oop goops) (aiscm core) (aiscm pulse) (aiscm tensorflow) (aiscm xorg))
(define s (make-session))
(define sample (tf-placeholder #:dtype <float> #:shape '(1 512)))
(define vec (tf-reshape sample (arr <int> 512)))
(define fourier (tf-rfft vec (arr <int> 512)))
(define spectrum (tf-real (tf-mul fourier (tf-conj fourier))))
(define y (* (% (indices 257 256) 256) (/ 16.0 256)))
(define record (make <pulse-record> #:typecode <float> #:channels 1 #:rate 11025))
(show
  (lambda _
    (let [(spec (run s (list (cons sample (read-audio record 512))) spectrum))]
      (where (gt spec y) 255 0))))
