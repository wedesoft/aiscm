#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1))

(define pi 3.1415926)
(define (sqr x) (* x x))
(define (int v) (inexact->exact (round v)))

(define n   24)
(define v  200)
(define r1  30)
(define r2  70)

(define f (/ r1 r2))

(define vertex (apply append (map (lambda (i)
  (let* [(a (/ (* 2 pi i) (/ n 2)))
         (ca (cos a))
         (sa (sin a))]
    (list (list (+ 100 (* ca r1)) (+ 100 (* sa r1)) (* -1 f sa v) (* f ca v))
          (list (+ 100 (* ca r2)) (+ 100 (* sa r2)) (* -1 sa v) (* ca v)))))
      (iota (/ n 2)))))

(define (clip x) (if (< x 0) (+ x n) (if (>= x n) (- x n) x)))

(define connect (apply append (map (lambda (i)
  (list (map (lambda (j) (clip (+ (* 2 i) j))) '(-2 -1 1 3 2))
        (map (lambda (j) (clip (+ (* 2 i) j))) '(-2 -1 0 2 3))))
  (iota (/ n 2)))))

(define distance (map (lambda (i l)
  (map (lambda (j)
    (let [(a (list-ref vertex i))
          (b (list-ref vertex j))]
      (sqrt (+ (sqr (- (car b) (car a))) (sqr (- (cadr b) (cadr a))))))) l)) (iota n) connect))

(define l 30)
(define dt 0.01)
(define g 20)
(define drag -0.1)
(define damp 5)
(define stiff 150)

(define main-window #t)

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 width height 0 -1 +1)
)

(define (change a b l)
  (let* [(x1  (car    a))
         (y1  (cadr   a))
         (x2  (car    b))
         (y2  (cadr   b))
         (vx1 (caddr  a))
         (vy1 (cadddr a))
         (vx2 (caddr  b))
         (vy2 (cadddr b))
         (d  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))
         (rv (/ (+ (* (- vx2 vx1) (- x2 x1)) (* (- vy2 vy1) (- y2 y1))) d))
         (s (- d l))
         (h (+ (* stiff s) (* damp rv)))
         (ax (* h (/ (- x2 x1) d)))
         (ay (* h (/ (- y2 y1) d)))]
    (list ax ay)))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (glPointSize 10)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (for-each (lambda (v) (gl-vertex (car v) (cadr v) 0)) vertex))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each (lambda (i)
      (for-each (lambda (j)
        (gl-vertex (car (list-ref vertex i)) (cadr (list-ref vertex i)) 0)
        (gl-vertex (car (list-ref vertex j)) (cadr (list-ref vertex j)) 0))
      (list-ref connect i)))
      (iota n)))
  (swap-buffers))

(define (on-idle)
   (let [( acc (map (lambda (i) (reduce (lambda (a b) (map + a b)) '(0 0) (map (lambda (j l) (change (list-ref vertex i) (list-ref vertex j) l)) (list-ref connect i) (list-ref distance i)))) (iota n)))]
     (set! vertex (map (lambda (v a) (list (+ (car v) (* (caddr v) dt))
                                           (+ (cadr v) (* (cadddr v) dt))
                                           (+ (caddr v) (* (+ (car a) (* drag (caddr v))) dt))
                                           (+ (cadddr v) (* (+ g (cadr a) (* drag (cadddr v))) dt)))) vertex acc))
     (set! vertex (map (lambda (v) (if (and (>= (cadr v) 470) (> (cadddr v) 0)) (list (car v) 470 0 0) v)) vertex)))
  (post-redisplay main-window))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "rigs of rods"))
(set-display-callback  on-display)
(set-reshape-callback  on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
