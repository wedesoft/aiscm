#!/usr/bin/env guile
!#
; http://twvideo01.ubm-us.net/o1/vault/gdc04/slides/using_verlet_integration.pdf
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define dt 0.08)
(define pi 3.141592653589793)

(define body '((-40 -20 1) ( 40 -20 1)
               (-40 -20 1) (-40  20 1)
               ( 40 -20 1) ( 40  20 1)
               (-40  20 1) ( 40  20 1)))

(define ship '(320 240 0))
(define ship_ (list (- 320 (* 5 dt)) 240 (/ (* 2 pi dt) 40)))
(define ground 470)

(define (matrix pose)
  (let [(x (car pose))
        (y (cadr pose))
        (a (caddr pose))]
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
  (gl-ortho 0 width height 0 -1 +1))

(define (on-idle)
  (let [ (update (map (lambda (v v_) (- (* 2 v) v_)) ship ship_))]
    (set! ship_ ship)
    (set! ship update)
    (post-redisplay)))

(define (gl-vertex-2d v) (gl-vertex (car v) (cadr v) 0))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (for-each gl-vertex-2d (map (cut dot (matrix ship) <>) body))
    (gl-vertex-2d (list 0 ground)) (gl-vertex-2d (list 640 ground)))
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

