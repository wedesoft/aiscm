(use-modules (aiscm core))
(define b (arr #f #f #t #t #f #t))
b
;#<sequence<bool>>:
;(#f #f #t #t #f #t)
(define u (to-type <ubyte> b))
u
;#<sequence<int<8,unsigned>>>:
;(0 0 1 1 0 1)
(to-type <bool> u)
u
;#<sequence<bool>>:
;(#f #f #t #t #f #t)
