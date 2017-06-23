(use-modules (aiscm sequence) (aiscm convolution) (aiscm complex))
(convolve (seq 0 0 2 2 2 2 0 0) (seq 1 0 -1))
;#<sequence<int<16,signed>>>:
;(0 2 2 0 0 -2 -2 0)
(convolve (seq 0 0 1 0 0 0 0 0 2 0) (seq +1 +i -1 -i))
;#<sequence<complex<int<16,signed>>>>:
;(1 0.0+1.0i -1 0.0-1.0i 0 0 2 0.0+2.0i -2 0.0-2.0i)
