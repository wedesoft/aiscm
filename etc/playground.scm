(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm convolution) (aiscm sequence) (aiscm operation) (aiscm expression) (aiscm loop) (srfi srfi-1) (aiscm command) (aiscm int) (aiscm variable) (aiscm asm) (aiscm rgb) (aiscm int) (aiscm jit) (aiscm scalar) (aiscm element) (ice-9 curried-definitions) (aiscm util))


(define-method (duplicate (a <indexer>) (b <convolution>))
  (letrec* [(kernel-loop (lambda (out data dstep kernel ksteps klowers kuppers kends)
              (let-parameter* [(dptr  <long> (array-pointer data))
                               (kptr  <long> (max (array-pointer kernel) (+ (array-pointer kernel) (last klowers))))
                               (klast <long> (+ (array-pointer kernel) (min (last kends) (last kuppers))))]
                (if (<= (dimensions (type kernel)) 1)
                  (let-parameter* [(tmp (typecode out) (* (rebase dptr data) (project (rebase kptr kernel))))]
                    (+= kptr (last ksteps))
                    (-= dptr dstep)
                    (each-element kptr klast (last ksteps)
                      (let-parameter* [(intermediate (typecode out) (* (rebase dptr data)
                                                                       (project (rebase kptr kernel))))]
                        (+= tmp intermediate)
                        (-= dptr dstep)))
                    (duplicate out tmp))
                  (kernel-loop out (rebase dptr data) dstep (project (rebase kptr kernel))
                               (all-but-last ksteps)
                               (all-but-last klowers)
                               (all-but-last kuppers)
                               (all-but-last kends))))))
            (data-loop (lambda (out data kernel kshape kstrides ksteps klowers kuppers kends)
              (let-parameter* [(offset <long> (>> (last kshape)))
                               (astep  <long> (* (stride out) (native-const <long> (size-of (typecode out)))))
                               (aptr   <long> (array-pointer out))
                               (alast  <long> (+ (array-pointer out) (* (dimension out) astep)))
                               (dstep  <long> (* (stride data) (native-const <long> (size-of (typecode data)))))
                               (dupper <long> (+ (array-pointer data) (* offset dstep)))
                               (dlast  <long> (+ (array-pointer data) (- (* (dimension data) dstep) dstep)))
                               (kstep  <long> (* (last kstrides) (native-const <long> (size-of (typecode kernel)))))
                               (klower <long> (+ (* (- offset (dimension data)) kstep) kstep))
                               (kend   <long> (* (last kshape) kstep))
                               (kupper <long> (+ (* offset kstep) kstep))]
                (each-element aptr alast astep
                        (let-parameter* [(dptr <long> (min dupper dlast))]
                          (if (<= (dimensions (type data)) 1)
                            (kernel-loop (project (rebase aptr out))
                                         (project (rebase dptr data))
                                         dstep
                                         kernel
                                         (cons kstep ksteps)
                                         (cons klower klowers)
                                         (cons kupper kuppers)
                                         (cons kend kends))
                            (data-loop (project (rebase aptr out))
                                       (project (rebase dptr data))
                                       kernel
                                       (all-but-last kshape)
                                       (all-but-last kstrides)
                                       (cons kstep ksteps)
                                       (cons klower klowers)
                                       (cons kupper kuppers)
                                       (cons kend kends)
                                       )))
                        (+= klower kstep)
                        (+= kupper kstep)
                        (+= dupper dstep)))))]
    (data-loop a
               (car (delegate b))
               (cadr (delegate b))
               (shape (cadr (delegate b)))
               (strides (cadr (delegate b)))
               '()
               '()
               '()
               '())))

(test-begin "playground")
(test-begin "1D convolution")
  (test-equal "convolution with one-element array"
    '(4 6 10) (to-list (convolve (seq 2 3 5) (seq 2))))
  (test-equal "do not read over array boundaries"
    '(0 0 0) (to-list (convolve (crop 3 (dump 1 (seq 1 0 0 0 1))) (seq 1 2 4))))
  (test-equal "convolution with 3-element impulse kernel"
    '(1 2 3 4 5) (to-list (convolve (seq 1 2 3 4 5) (seq 0 1 0))))
  (test-equal "convolution with 32-bit integers"
    '(1 2 3 4 5) (to-list (convolve (seq <int> 1 2 3 4 5) (seq <int> 0 1 0))))
  (test-equal "convolution with 3-element shift-left kernel"
    '(2 3 0) (to-list (convolve (seq 1 2 3) (seq 1 0 0))))
  (test-equal "convolution with 3-element shift-right kernel"
    '(0 1 2) (to-list (convolve (seq 1 2 3) (seq 0 0 1))))
  (test-equal "use stride of data"
    '(1 2) (to-list (convolve (get (roll (arr (1 0) (2 0))) 0) (seq 1))))
  (test-equal "use stride of kernel"
    '(1 2) (to-list (convolve (seq 0 1) (get (roll (arr (1 0) (2 0))) 0))))
(test-end "1D convolution")

(test-begin "convolution with composite values")
  (test-equal "RGB-scalar convolution"
    (list (rgb 4 6 10)) (to-list (convolve (seq (rgb 2 3 5)) (seq 2))))
(test-end "convolution with composite values")

(test-begin "2D convolution")
  (test-equal "trivial 2D convolution"
    '((2 3 5) (7 11 13)) (to-list (convolve (arr (2 3 5) (7 11 13)) (arr (1)))))
  (test-equal "test impulse in last dimension"
    '((2 3 5) (7 11 13)) (to-list (convolve (arr (2 3 5) (7 11 13)) (arr (0 1 0)))))
  (test-skip 1)
  (test-equal "test impulse in first dimension"
    '((2 3) (5 7) (11 13)) (to-list (convolve (arr (2 3) (5 7) (11 13)) (arr (0) (1) (0)))))
(test-end "2D convolution")
(test-end "playground")
