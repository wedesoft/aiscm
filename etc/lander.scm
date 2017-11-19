#!/usr/bin/env guile
!#
; http://www.cs.unc.edu/~lin/COMP768-F07/
; http://www.cs.unc.edu/~lin/COMP768-F07/LEC/rbd1.pdf
; http://www.cs.unc.edu/~lin/COMP768-F07/LEC/rbd2.pdf
(use-modules (oop goops) (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26) (ice-9 format))

(define dt 0.04)
(define scale (/ 1 dt))
(define pi 3.141592653589793)
(define g '(0 0.1 0))
(define engine '(0 -0.3 0))
(define nozzle -0.05)
(define thrust 0)
(define rcs 0)
(define m 1)
(define t (* (/ 1 12) (+ (* 4 4) (* 2 2))))

(define-method (+ (a <list>) (b <list>)) (map + a b))
(define-method (- (a <list>) (b <list>)) (map - a b))
(define-method (* (a <real>) (b <list>)) (map (cut * a <>) b))
(define-method (* (a <list>) (b <real>)) (map (cut * <> b) a))

(define corners '((-4 -2 1) (4 -2 1) (-4 2 1) (4 2 1)))
(define body '((-4 -2 1) ( 4 -2 1)
               (-4 -2 1) (-4  2 1)
               ( 4 -2 1) ( 4  2 1)
               (-4  2 1) ( 4  2 1)))

(define position '(32 24 1))
(define position_ '(32 24 1))
(define angle -0.3)
(define angle_ -0.29)
(define ground 40.0)

(define (cross r n)
  (- (* (car r) (cadr n)) (* (cadr r) (car n))))

(define (spin p dangle)
  (* dangle (list (- (cadr p)) (car p) 0)))

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
    (let* [(outer     (map (cut dot (matrix position angle) <>) body))
           (collision (find (lambda (p) (>= (cadr p) ground)) outer))]
      (if (and collision (>= (cadr position) (cadr position_)))
        (let* [(r    (- collision position))
               (vrel (cadr (+ (- position position_) (spin (- collision position) (- angle angle_)))))
               (j    (/ (* -2 vrel) (+ (/ 1 m) (* (car r) (car r) (/ 1 t)))))
               (dv   (/ j m))
               (dw   (* (car r) (/ j t)))]
          (format #t "vrel: ~a, dangle: ~a, j: ~a, dv: ~a, dw: ~a~&" vrel (- angle angle_) j dv dw)
          (set! position_ (list (car position_) (- (cadr position_) dv)))
          (set! angle_ (- angle_ dw))
         (format #t "vrel': ~a~&" (cadr (+ (- position position_) (spin (- collision position) (- angle angle_))))))))
    (post-redisplay)))

(define (gl-vertex-2d v) (gl-vertex (car v) (cadr v) 0))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each gl-vertex-2d (map (cut dot (matrix position angle) <>) body))
    (gl-vertex-2d (list 0 ground)) (gl-vertex-2d (list 640 ground))
    (gl-color 0 0 1)
    (gl-vertex-2d position) (gl-vertex-2d (- (* (1+ scale) position) (* scale position_)))
    (for-each (lambda (p)
      (gl-vertex-2d p)
      (gl-vertex-2d (+ p (* scale (+ (- position position_) (spin (- p position) (- angle angle_)))))))
      (map (cut dot (matrix position angle) <>) corners)))
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
