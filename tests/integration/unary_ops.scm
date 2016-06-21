(use-modules (aiscm sequence) (aiscm pointer) (aiscm int) (aiscm jit))
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
