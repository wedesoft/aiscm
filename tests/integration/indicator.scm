(use-modules (aiscm core) (aiscm xorg) (aiscm util))
(define pi 3.1415926)
(define idx (index 640 480))
(define x (- (% idx 640) 320))
(define y (- (/ idx 640) 240))
(define r (sqrt (+ (* x x) (* y y))))
(define m (&& (ge r 200) (le r 240)))
(define a (* (+ (atan y x) pi) (/ 255 (* 2 pi))))
(define p (where m a 255))
(define t (clock))
(show (lambda (dsp) (where (lt p (abs (- 255 (modulo (inexact->exact (round (* (elapsed t) 150))) 510)))) 255 0)))