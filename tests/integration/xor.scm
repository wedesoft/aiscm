(use-modules (aiscm core) (aiscm tensorflow) (ice-9 format))

(define features (arr <float> (0 0) (0 1) (1 0) (1 1)))
(define labels (arr <float> (0) (1) (1) (0)))

(define s (make-session))

(define x (tf-placeholder #:dtype <float> #:shape '(-1 2) #:name "x"))
(define y (tf-placeholder #:dtype <float> #:shape '(-1 1) #:name "y"))
(define m1 (tf-variable #:dtype <float> #:shape '(2 2) #:name "m1"))
(run s '() (tf-assign m1 (tf-truncated-normal (tf-shape m1) #:dtype <float>)))
(define b1 (tf-variable #:dtype <float> #:shape '(2) #:name "b1"))
(run s '() (tf-assign b1 (fill <float> '(2) 0.0)))
(define h1 (tf-tanh (tf-add (tf-mat-mul x m1) b1)))

(define m2 (tf-variable #:dtype <float> #:shape '(2 1) #:name "m2"))
(run s '() (tf-assign m2 (tf-truncated-normal (tf-shape m2) #:dtype <float>)))
(define b2 (tf-variable #:dtype <float> #:shape '(1) #:name "b2"))
(run s '() (tf-assign b2 (arr <float> 0)))
(define ys (tf-sigmoid (tf-add (tf-mat-mul h1 m2) b2) #:name "ys"))

(define one (tf-cast 1 #:DstT <float>))
(define cost (tf-neg (tf-mean (tf-add (tf-mul y (tf-log ys)) (tf-mul (tf-sub one y) (tf-log (tf-sub one ys)))) (arr <int> 0 1))))

(define vars (list m1 b1 m2 b2))
(define gradients (tf-add-gradient cost vars))

(define alpha (tf-cast 1.0 #:DstT <float>))
(define step (map (lambda (v g) (tf-assign v (tf-sub v (tf-mul g alpha)))) vars gradients))

(for-each (lambda _ (run s (list (cons x features) (cons y labels)) step)) (iota 250))
(format #t "~a~&" (run s (list (cons x features)) ys))
