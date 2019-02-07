(use-modules (oop goops) (aiscm core) (aiscm v4l2) (aiscm xorg))
(define v (make <v4l2>))
(define idx (apply indices (shape v)))
(define width (cadr (shape idx)))
(define height (car (shape idx)))
(define x (% idx width))
(define y (/ idx width))
(define cx (/ width 2))
(define cy (/ height 2))
(define (sqr x) (* x x))
(define dx (- x cx))
(define dy (- y cy))
(define r (sqrt (+ (sqr dx) (sqr dy))))
(define cs (/ dx (major r 1)))
(define sn (/ dy (major r 1)))
(define z 50)
(show
  (lambda _
    (let [(img (to-array (read-image v)))]
      (set! z (min 200 (+ 10 z)))
      (let* [(o  (where (lt r z) (* r (- 1 (/ 50 z))) 0))
             (wx (to-type <int> (- x (* cs o))))
             (wy (to-type <int> (- y (* sn o))))]
        (warp img wx wy)))))
