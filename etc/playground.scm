(use-modules (srfi srfi-64) (oop goops) (aiscm convolution) (aiscm sequence) (aiscm operation) (aiscm expression) (aiscm loop) (srfi srfi-1) (aiscm command) (aiscm int) (aiscm variable) (aiscm asm))

(define-method (duplicate (a <indexer>) (b <convolution>))
  (let [(data (car (delegate b)))
        (kernel (cadr (delegate b)))]
  (if (null? (shape kernel))
    (duplicate a (* data kernel))
    (let-parameter* [(offset <long> (>> (dimension kernel)))]
    (let [(offset   (parameter <long>))
          (a0       (parameter <long>))
          (astep    (parameter <long>))
          (d0       (parameter <long>))
          (d1       (parameter <long>))
          (dstep    (parameter <long>))
          (k0       (parameter <long>))
          (kstep    (parameter <long>))
          (kend     (parameter <long>))
          (dlast    (parameter <long>))
          (klower   (parameter <long>))
          (kupper   (parameter <long>))
          (upper    (parameter <long>))]
      (append (list (duplicate offset (>> (dimension kernel)))
                    (duplicate a0 (array-pointer a))
                    (duplicate astep (* (stride a) (native-const <long> (size-of (typecode a)))))
                    (duplicate dstep (* (stride data) (native-const <long> (size-of (typecode data)))))
                    (duplicate d0 (+ (array-pointer data) (* offset dstep)))
                    (duplicate dlast (+ (array-pointer data) (- (* (dimension data) dstep) dstep)))
                    (duplicate kstep (* (stride kernel) (native-const <long> (size-of (typecode kernel)))))
                    (duplicate klower (+ (array-pointer kernel) (+ (* (- offset (dimension data)) kstep) kstep)))
                    (duplicate kend (+ (array-pointer kernel) (* (dimension kernel) kstep)))
                    (duplicate kupper (+ (array-pointer kernel) (+ (* offset kstep) kstep))))
              (repeat 0 (dimension a)
                      (duplicate k0 (max (array-pointer kernel) klower))
                      (duplicate d1 (min d0 dlast))
                      (let [(tmp (parameter (typecode a)))]
                        (append (duplicate tmp (* (project (rebase d1 data)) (project (rebase k0 kernel))))
                                (+= k0 kstep) (-= d1 dstep)
                                (duplicate upper (min kend kupper))
                                (each-element k0 upper kstep
                                        (+= tmp (* (project (rebase d1 data)) (project (rebase k0 kernel))))
                                        (-= d1 dstep))
                                (duplicate (project (rebase a0 a)) tmp)))
                      (+= kupper kstep)
                      (+= klower kstep)
                      (+= a0 astep)
                      (+= d0 dstep))))))))

(test-begin "playground")
(test-begin "convolution")
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
(test-end "convolution")
(test-end "playground")
