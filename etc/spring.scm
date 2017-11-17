#!/usr/bin/env guile
!#
; https://www.saylor.org/site/wp-content/uploads/2011/06/MA221-6.1.pdf
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define ox 320)
(define oy 20)
(define cx 320)
(define cy 100)
(define vy 0)
(define l 250)
(define k/m 200)
(define d/m 2)

;(define dt 0.08)
(define dt 0.08)

(define main-window #f)

(define (euler)
  (define ay (- (* k/m (- l (- cy oy))) (* d/m vy)))
  (set! vy (+ vy (* ay dt)))
  (set! cy (+ cy (* vy dt))))

(define (verlet)
  (define ay (- (* k/m (- l (- cy oy))) (* d/m vy)))
  (set! cy (+ cy (* vy dt) (* 0.5 ay (* dt dt))))
  (let [(ay_ (- (* k/m (- l (- cy oy))) (* d/m vy)))]
    (set! vy (+ vy (* 0.5 (+ ay ay_) dt)))))

(define (on-idle)
  (verlet)
  ;(euler)
  (post-redisplay))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (gl-vertex ox oy)
    (gl-vertex cx cy))
  (glPointSize 5)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (gl-vertex cx cy))
  (swap-buffers))

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 width height 0 -1 +1))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "spring"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
