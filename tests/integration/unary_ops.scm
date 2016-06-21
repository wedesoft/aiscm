(use-modules (aiscm sequence) (aiscm pointer) (aiscm int) (aiscm jit) (aiscm rgb) (aiscm complex))
(- (seq <int> 2 3 5))
;#<sequence<int<32,signed>>>:
;(-2 -3 -5)
(~ (seq <byte> -128 -3 -2 -1 0 1 2 127))
;#<sequence<int<8,signed>>>:
;(127 2 1 0 -1 -2 -3 -128)
(=0 (seq -2 -1 0 1 2))
;#<sequence<bool>>:
;(#f #f #t #f #f)
(!=0 (seq -2 -1 0 1 2))
;#<sequence<bool>>:
;(#t #t #f #t #t)
(red (seq (rgb 2 3 5) (rgb 3 5 7)))
;#<sequence<int<8,unsigned>>>:
;(2 3)
(green (seq (rgb 2 3 5) (rgb 3 5 7)))
;#<sequence<int<8,unsigned>>>:
;(3 5)
(blue (seq (rgb 2 3 5) (rgb 3 5 7)))
;#<sequence<int<8,unsigned>>>:
;(5 7)
(red (seq 2 3 5))
;#<sequence<int<8,unsigned>>>:
;(2 3 5)
(real-part (seq (complex <sint>) 2+3i 5+7i))
;#<sequence<int<16,signed>>>:
;(2 5)
(real-part (seq 2 3 5))
;#<sequence<int<8,unsigned>>>:
;(2 3 5)
(imag-part (seq (complex <sint>) 2+3i 5+7i))
;#<sequence<int<16,signed>>>:
;(3 7)
(imag-part (seq 2 3 5))
;#<sequence<int<8,unsigned>>>:
;(0 0 0)
(conj (seq (complex <byte>) 2+3i 5+7i))
;#<sequence<complex<int<8,signed>>>:
;(2.0-3.0i 5.0-7.0i)
(conj (seq 2 3 5))
;#<sequence<int<8,unsigned>>>:
;(2 3 5)
