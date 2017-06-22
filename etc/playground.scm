(use-modules (srfi srfi-64))
(use-modules (oop goops) (aiscm convolution) (aiscm sequence) (aiscm operation) (aiscm expression) (aiscm loop) (srfi srfi-1) (aiscm command) (aiscm int) (aiscm variable) (aiscm asm) (aiscm rgb) (aiscm int) (aiscm jit) (aiscm scalar) (aiscm element) (ice-9 curried-definitions))

(define-method (duplicate (a <indexer>) (b <convolution>))
  (let [(data (car (delegate b)))
        (kernel (cadr (delegate b)))]
  (if (null? (shape kernel))
    (duplicate a (* data kernel))
    (let-parameter* [(offset <long> (>> (dimension kernel)))
                     (astep  <long> (* (stride a) (native-const <long> (size-of (typecode a)))))
                     (aptr   <long> (array-pointer a))
                     (dstep  <long> (* (stride data) (native-const <long> (size-of (typecode data)))))
                     (dupper <long> (+ (array-pointer data) (* offset dstep)))
                     (dlast  <long> (+ (array-pointer data) (- (* (dimension data) dstep) dstep)))
                     (kstep  <long> (* (stride kernel) (native-const <long> (size-of (typecode kernel)))))
                     (klower <long> (+ (array-pointer kernel) (+ (* (- offset (dimension data)) kstep) kstep)))
                     (kend   <long> (+ (array-pointer kernel) (* (dimension kernel) kstep)))
                     (kupper <long> (+ (array-pointer kernel) (+ (* offset kstep) kstep)))]
      (repeat 0 (dimension a)
              (let-parameter* [(dptr  <long> (min dupper dlast))
                               (kptr  <long> (max (array-pointer kernel) klower))
                               (klast <long> (min kend kupper))
                               (tmp   (typecode a) (* (project (rebase dptr data))
                                                      (project (rebase kptr kernel))))]
                (+= kptr kstep)
                (-= dptr dstep)
                (each-element kptr klast kstep
                        (let-parameter* [(intermediate (typecode a) (* (project (rebase dptr data)) (project (rebase kptr kernel))))]
                          (+= tmp intermediate))
                        (-= dptr dstep))
                (duplicate (project (rebase aptr a)) tmp))
              (+= kupper kstep)
              (+= klower kstep)
              (+= aptr astep)
              (+= dupper dstep))))))

(test-begin "playground")
(test-begin "1D convolution")
  (test-equal "trivial convolution"
    '(2 3 5) (to-list (convolve (seq 2 3 5) 1)))
  (test-equal "use convolution to scale values"
    '(4 6 10) (to-list (convolve (seq 2 3 5) 2)))
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
(test-end "1D convolution")

(test-begin "convolution with composite values")
  (test-equal "RGB-scalar convolution"
    (list (rgb 4 6 10)) (to-list (convolve (seq (rgb 2 3 5)) (seq 2))))
(test-end "convolution with composite values")
(test-end "playground")
