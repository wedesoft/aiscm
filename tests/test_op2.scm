(use-modules (oop goops)
             (aiscm util)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm asm)
             (aiscm jit)
             (aiscm sequence)
             (aiscm op)
             (guile-tap))
(planned-tests 41)
(ok (equal? '((3 4 5) (7 8 9)) (to-list (+ (seq 0 1) (arr (3 4 5) (6 7 8)))))
    "add 1D and 2D array")
(ok (equal? '((3 4 5) (7 8 9)) (to-list (+ (arr (3 4 5) (6 7 8)) (seq 0 1))))
    "add 2D and 1D array")
(ok (equal?  '((((2 2) (2 2)) ((2 2) (2 2))) (((2 2) (2 2)) ((2 2) (2 2))))
             (to-list (+ 1 (fill <int> '(2 2 2 2) 1))))
    "add 1 to 4D array")
(ok (equal? '((2 4) (6 8)) (to-list (* 2 (arr (1 2) (3 4)))))
    "scalar-array multiplication")
(ok (equal? '((256 256) (256 256)) (to-list (* 256 (arr (1 1) (1 1)))))
    "correct handling of strides with 2D short integer array")
(ok (equal? '(1 4) (to-list (duplicate (project (roll (arr (1 2 3) (4 5 6)))))))
    "'duplicate' creates copy of slice")
(ok (equal? '((1 3) (2 4)) (to-list (duplicate (roll (arr (1 2) (3 4))))))
    "'duplicate' creates copy of 2D array")
(ok (equal? '(1 2) (strides (duplicate (roll (arr (1 2) (3 4))))))
    "'duplicate' creates a compact copy of the array")
(ok (equal? '(-2 3 5) (to-list (to-type <byte> (seq -2 3 5))))
    "trivial type conversion")
(ok (eq? <int> (typecode (to-type <int> (seq 2 3 5))))
    "element-wise type conversion converts to target type")
(ok (equal? '(2 3 5) (to-list (to-type <uint> (seq 2 3 5))))
    "element-wise conversion preserves values when increasing integer size")
(ok (equal? '(-2 3 5) (to-list (to-type <int> (seq -2 3 5))))
    "element-wise conversion preserves sign when increasing integer size")
(ok (equal? '(254 3 5) (to-list (to-type <ubyte> (seq -2 3 5))))
    "typecasting to corresponding signed integer type")
(ok (equal? '(255 0 1) (to-list (to-type <ubyte> (seq 255 256 257))))
    "typecasting to smaller integer type")
(ok (equal? '(253 252 250) (to-list (~ (seq 2 3 5))))
    "bitwise negation of array")
(ok (equal? '(#f #t #f) (to-list (=0 (seq -1 0 1))))
    "compare bytes with zero")
(ok (equal? '(#f #t #f) (to-list (=0 (seq <int> -1 0 1))))
    "compare integers with zero")
(ok (equal? '(#t #f #t) (to-list (!=0 (seq -1 0 1))))
    "check whether bytes are not zero")
(ok (equal? '(#f #t #f) (to-list (! (seq #t #f #t))))
    "element-wise not for booleans")
(ok (equal? '(#f #t) (to-list (= (seq 3 4) (seq 5 4))))
    "element-wise array-array comparison")
(ok (equal? '(#f #t) (to-list (= 4 (seq 5 4))))
    "element-wise scalar-array comparison")
(ok (equal? '(#f #t #f) (to-list (= (seq 3 4 5) 4)))
    "element-wise array-scalar comparison")
(ok (equal? '(#t #f #t) (to-list (!= (seq 3 4 5) 4)))
    "element-wise not-equal")
(ok (equal? '(#t #f #f) (to-list (< (seq 3 4 5) 4)))
    "element-wise lower-than")
(ok (equal? '(#t #t #f) (to-list (<= (seq 3 4 5) 4)))
    "element-wise lower-equal")
(ok (equal? '(#f #f #t) (to-list (> (seq 3 4 5) 4)))
    "element-wise greater-than")
(ok (equal? '(#f #t #t) (to-list (>= (seq 3 4 5) 4)))
    "element-wise greater-equal")
(ok (equal? '(#t #f #f) (to-list (< (seq -1 0 1) 0)))
    "element-wise lower-than of signed and unsigned bytes")
(ok (equal? '(#t #t #f) (to-list (<= (seq -1 0 1) 0)))
    "element-wise lower-equal of signed and unsigned bytes")
(ok (equal? '(#f #f #t) (to-list (> (seq -1 0 1) 0)))
    "element-wise greater-than of signed and unsigned bytes")
(ok (equal? '(#f #t #t) (to-list (>= (seq -1 0 1) 0)))
    "element-wise greater-equal of signed and unsigned bytes")
(ok (equal? '(#f #f #f) (to-list (< (seq 1 2 128) -1)))
    "element-wise lower-than of unsigned and signed bytes")
(ok (equal? '(#f #f #f) (to-list (<= (seq 1 2 128) -1)))
    "element-wise lower-equal of unsigned and signed bytes")
(ok (equal? '(#t #t #t) (to-list (> (seq 1 2 128) -1)))
    "element-wise greater-than of unsigned and signed bytes")
(ok (equal? '(#t #t #t) (to-list (>= (seq 1 2 128) -1)))
    "element-wise greater-equal of unsigned and signed bytes")
(ok (equal? '(3 0 1) (to-list (& (seq 3 4 5) 3)))
    "element-wise bit-wise and")
(ok (equal? '(3 7 7) (to-list (| 3 (seq 3 4 5))))
    "element-wise bit-wise or")
(ok (equal? '(1 7 1) (to-list (^ (seq 2 3 4) (seq 3 4 5))))
    "element-wise bit-wise xor")
(ok (equal? '(#f #f #f #t) (to-list (&& (seq #f #t #f #t) (seq #f #f #t #t))))
    "element-wise and")
(ok (equal? '(#f #t #t #t) (to-list (|| (seq #f #t #f #t) (seq #f #f #t #t))))
    "element-wise or")
(ok (equal? '(1 2 -3) (to-list (/ (seq 3 6 -9) 3)))
    "element-wise signed byte division")
