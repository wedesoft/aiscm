(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

;(define-tensor (name arr size) (tensor [size] i (get arr i)))

;(define-method (native-type2 value) (native-type value))
;(define-method (native-type2 (value <integer>)) <int>)
;
;(define-syntax-rule (define-tensor (name args ...) expression)
;  (define-method (name args ...)
;    (let [(fun (jit (map native-type2 (list args ...)) (lambda (args ...) expression)))]
;      (add-method! name
;                   (make <method> #:specializers (map class-of (list args ...))
;                                  #:procedure fun))
;      (name args ...))))

;(define-tensor (test x) (+ x x))

;(define-class <index> ())
;
;(define-class <lookup> ()
;  (term #:init-keyword #:term #:getter term)
;  (index #:init-keyword #:index #:getter index))
;
;
;(define-method (get (value <multiarray<>>) (index <index>))
;  (make <lookup> #:term value #:getter index))
;
;(define-method (shape (value <lookup>) (i <index>))
;  (shape (term value)))
;
;(define-method (subst (value <lookup>) (i <index>) (j <integer>))
;  (get (term value) j))
;
;(define-syntax-rule (tensor i expression)
;  (let* [(i (make <index>))
;         (t expression)]
;    (map (lambda (j) (subst t i j)) (iota (last (shape t i))))))
;
;(define m (arr 1 2 3))
;(tensor i (get m i))
;
;(define m (arr (1 2 3) (4 5 6)))
;(tensor i (tensor j (get m i j)))

(define-class <lambda> ()
  (term #:init-keyword #:term #:getter term)
  (size #:init-keyword #:size #:getter size))

(define-method (write (self <lambda>) port)
  (format port "(lamb ~a ~a)" (term self) (size self)))

(define-class <lookup> ()
  (term #:init-keyword #:term #:getter term)
  (stride #:init-keyword #:stride #:getter stride))

(define-method (write (self <lookup>) port)
  (format port "(lookup ~a ~a)" (term self) (stride self)))

(define-method (lamb term size)
  (make <lambda> #:size size #:term term))

(define-method (lookup term stride)
  (make <lookup> #:term term #:stride stride))

(define-method (lookup (t <lambda>) stride)
  (lamb (lookup (term t) stride) (size t)))

(define-method (fetch (p <foreign>))
  (bytevector-u8-ref (pointer->bytevector p 1) 0))

(define-method (+ (a <foreign>) (b <integer>))
  (make-pointer (+ (pointer-address a) b)))

(define (arr->tensor a)
  (if (zero? (dimensions a))
    (memory a)
    (lamb (lookup (arr->tensor (project a)) (last (strides a))) (last (shape a)))))

(define m (arr 1 2 3))
(arr->tensor m)

(define n (arr (1 2 3) (4 5 6)))
(arr->tensor n)
