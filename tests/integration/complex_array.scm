(use-modules (aiscm complex) (aiscm sequence) (aiscm pointer) (aiscm int))
(define c (seq 2+3i 5+7i))
c
;#<sequence<complex<int<8,unsigned>>>:
;(2.0+3.0i 5.0+7.0i)
(real-part c)
;#<sequence<int<8,unsigned>>>:
;(2 5)
(imag-part c)
;#<sequence<int<8,unsigned>>>:
;(3 7)
(complex (seq 2 5) (seq 3 7))
;#<sequence<complex<int<8,unsigned>>>:
;(2.0+3.0i 5.0+7.0i)
(conj c)
;#<sequence<complex<int<8,unsigned>>>:
;(2.0+253.0i 5.0+249.0i)
(conj (to-type (complex <byte>) c))
;#<sequence<complex<int<8,signed>>>:
;(2.0-3.0i 5.0-7.0i)
