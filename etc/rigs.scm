#!/usr/bin/env guile
!#
; https://www.youtube.com/watch?v=GnzzmA9x1q4
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define pi 3.141592653589793)
(define g 20)
(define drag -0.5)
(define damp 2)
(define stiff 200)
(define (sqr x) (* x x))
(define (int v) (inexact->exact (round v)))
(define (vector a b) (map - b a))
(define (distance a b) (sqrt (apply + (map sqr (vector a b)))))

(define dt 0.05)
(define n   16)
(define v  200)
(define r1  30)
(define r2  70)
(define cx 100)
(define cy 100)

(define n2 (/ n 2))

; speed values
; accelerations for given positions and speeds
; 4th order Runger-Kutta 4

(define angles (map (cut * 2 (/ pi n2) <>) (iota n)))
(define radius (append (make-list n2 r1) (make-list n2 r2)))

(define vertex (map (lambda (a r) (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) ) angles radius))
(define speed (make-list n '(0 0)))

(define (inner-index index offset) (remainder (+ index offset n2) n2))
(define (outer-index index offset) (+ (remainder (+ index offset n2) n2) n2))
(define (neighbours index)
  (delete index
          (append (map (cut inner-index index <>) '(-1 0 1))
                  (map (cut outer-index index <>) '(-1 0 1)))))
(define edge (map neighbours (iota n)))

(define (adjacent vertex) (map (cut map (cut list-ref vertex <>) <>) edge))
(define nominal (map (lambda (u vs) (map (cut distance u <>) vs)) vertex (adjacent vertex)))

(define (scale node factor) (map (cut * factor <>) node))
(define (scale-all nodes dt) (map (cut scale <> dt) nodes))

(define (add node delta) (map + node delta))
(define (add-all nodes delta) (map add nodes delta))

(define (relative-speed u du v dv)
  (/ (apply + (map * (vector u v) (vector du dv))) (distance u v)))

(define (spring u du v dv l)
  (let* [(distance   (distance u v))
         (displace   (- distance l))
         (speed      (relative-speed u du v dv))
         (accelerate (+ (* stiff displace) (* damp speed)))]
    (scale (vector u v) (/ accelerate distance))))

(define (acceleration vertex speed vs dvs ls)
  (add (reduce add '(0 0) (map (cut spring vertex speed <...>) vs dvs ls))
       (add (scale speed drag) (list 0 g))))

(define (acceleration-all vertex speed)
  (map acceleration vertex speed (adjacent vertex) (adjacent speed) nominal))

(define (state-all vertex speed) (map append vertex speed))
(define (vertex-state state) (map (cut take <> 2) state))
(define (speed-state state) (map (cut drop <> 2) state))

(define (change-all state)
  (map append (speed-state state) (acceleration-all (vertex-state state) (speed-state state))))

(define main-window #f)

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 width height 0 -1 +1))

(define (on-idle)
  (let* [(state  (state-all vertex speed))
         (k1     (change-all state))
         (k2     (change-all (add-all state (scale-all k1 (/ dt 2)))))
         (k3     (change-all (add-all state (scale-all k2 (/ dt 2)))))
         (k4     (change-all (add-all state (scale-all k3 dt      ))))
         (update (add-all state (reduce add-all #f (map scale-all (list k1 k2 k3 k4) (list (/ dt 6) (/ dt 3) (/ dt 3) (/ dt 6))))))]
    (set! speed (speed-state update))
    (set! vertex (vertex-state update)))
  (set! vertex (map (lambda (v s) (if (and (>= (cadr v) 470) (> (cadr s) 0)) (list (car v) 470) v)) vertex speed))
  (set! speed (map (lambda (v s) (if (and (>= (cadr v) 470) (> (cadr s) 0)) (list 0 0) s)) vertex speed))
  (post-redisplay))

(define (gl-vertex-2d v) (gl-vertex (car v) (cadr v) 0))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (glPointSize 5)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (for-each gl-vertex-2d vertex))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each (lambda (u vs) (for-each (lambda (v) (gl-vertex-2d u) (gl-vertex-2d v)) vs)) vertex (adjacent vertex)))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "rigs of rods"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
