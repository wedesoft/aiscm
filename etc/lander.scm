#!/usr/bin/env guile
!#
; http://twvideo01.ubm-us.net/o1/vault/gdc04/slides/using_verlet_integration.pdf
(use-modules (oop goops) (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define dt 0.08)
(define pi 3.141592653589793)
(define g '(0 0.1 0))
(define engine '(0 -0.3 0))
(define nozzle -0.05)
(define thrust 0)
(define rcs 0)

(define-method (+ (a <list>) (b <list>)) (map + a b))
(define-method (- (a <list>) (b <list>)) (map - a b))
(define-method (* (a <real>) (b <list>)) (map (cut * a <>) b))
(define-method (* (a <list>) (b <real>)) (map (cut * <> b) a))

(define body '((-4 -2 1) ( 4 -2 1)
               (-5 -2 1) (-4  2 1)
               ( 4 -3 1) ( 4  2 1)
               (-4  2 1) ( 4  2 1)))

(define position '(32 24 1))
(define position_ '(32 24 1))
(define angle 0)
(define angle_ 0)
(define ground 47.0)

(define (matrix position angle)
  (let [(x (car position))
        (y (cadr position))
        (a angle)]
    (list (list (cos a) (- (sin a)) x)
          (list (sin a)    (cos a)  y)
          (list       0          0  1))))

(define (dot mat vec)
  (map (lambda (row) (reduce + 0 (map * row vec))) mat))

(define main-window #f)

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 64 48 0 -1 +1))

(define (on-key keycode x y)
  (let [(c (integer->char keycode))]
    (case c
      ((#\w) (set! thrust (- 1 thrust)))
      ((#\a) (set! rcs  1))
      ((#\d) (set! rcs -1))
      ((#\s) (set! rcs  0)))))

(define (on-idle)
  (let* [(acceleration (+ g (* thrust (dot (matrix position angle) engine))))
         (angular      (* rcs nozzle))
         (position+    (+ (- (* 2 position) position_) (* acceleration dt dt)))
         (angle+       (+ (- (* 2 angle) angle_) (* angular dt dt)))]
    (set! position_ position)
    (set! position position+)
    (set! angle_ angle)
    (set! angle angle+)
    (post-redisplay)))

(define (gl-vertex-2d v) (gl-vertex (car v) (cadr v) 0))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each gl-vertex-2d (map (cut dot (matrix position angle) <>) body))
    (gl-vertex-2d (list 0 ground)) (gl-vertex-2d (list 640 ground)))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "rigs"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set-keyboard-callback on-key)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
