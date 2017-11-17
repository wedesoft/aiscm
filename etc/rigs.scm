#!/usr/bin/env guile
!#
; https://www.youtube.com/watch?v=GnzzmA9x1q4
; http://docs.rigsofrods.org/vehicle-creation/wheels-axles-steering/
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define pi 3.141592653589793)
(define g 100)
(define drag -0.2)
(define damp  50)
(define stiff 700)
(define (sqr x) (* x x))
(define (int v) (inexact->exact (round v)))
(define (vector a b) (map - b a))
(define (distance a b) (sqrt (apply + (map sqr (vector a b)))))

(define dt  0.010)
(define n      13)
(define n1 (1- n))
(define r      50)
(define v       0)
(define cx    320)
(define cy    400)
(define gs  -1900)
(define stick 3000)
(define w    0.98)

(define (friction v)
  (max gs (- v (* stick dt))))

(define angles (map (cut * 2 (/ pi n1) <>) (iota n1)))
(define radius (append (make-list n1 r)))

(define vertex (cons (list cx cy) (map (lambda (a) (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) ) angles)))
(define speed (cons '(0 0) (map (lambda (a) (list (* v -1 (sin a)) (* v (cos a))) ) angles)))

(define (next-index index offset) (1+ (remainder (+ (1- index) n1 offset) n1)))
(define (neighbours index) (cons 0 (map (cut next-index index <>) '(-1 1))))
(define edge (cons (iota n1 1) (map neighbours (iota n1 1))))

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
  (let [(acc (map acceleration vertex speed (adjacent vertex) (adjacent speed) nominal))]
    (cons (add (scale (car acc) (- 1 w)) (scale (list 0 g) w)) (cdr acc))))

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
  (for-each (lambda _
    (let* [(state  (state-all vertex speed))
           (k1     (change-all state))
           (k2     (change-all (add-all state (scale-all k1 (/ dt 2)))))
           (k3     (change-all (add-all state (scale-all k2 (/ dt 2)))))
           (k4     (change-all (add-all state (scale-all k3 dt      ))))
           (update (add-all state (reduce add-all #f (map scale-all (list k1 k2 k3 k4) (list (/ dt 6) (/ dt 3) (/ dt 3) (/ dt 6))))))]
      (set! speed (speed-state update))
      (set! vertex (vertex-state update))
      (set! vertex (map (lambda (v s) (if (and (>= (cadr v) 480) (>= (cadr s) 0)) (list (car v) 480) v)) vertex speed))
      (set! speed (map (lambda (v s) (if (and (>= (cadr v) 480) (>= (cadr s) 0)) (list (friction (car s)) 0) s)) vertex speed))
      (set! vertex (cons (list cx (cadar vertex)) (cdr vertex)))
      (set! speed (cons (list 0 (cadar speed)) (cdr speed)))
      (post-redisplay))) (iota 3)))

(define (gl-vertex-2d v) (gl-vertex (car v) (cadr v) 0))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each (lambda (u vs) (for-each (lambda (v) (gl-vertex-2d u) (gl-vertex-2d v)) vs)) vertex (adjacent vertex)))
  (glPointSize 5)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (for-each gl-vertex-2d vertex))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "rigs"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
