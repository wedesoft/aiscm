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

(define-class <tensor> ())

(define-class <index> (<tensor>)
  (sym #:init-keyword #:sym #:getter sym)
  (size #:init-keyword #:size #:getter size))

(define-method (write (self <index>) port)
  (format port "(index ~a ~a)" (size self) (sym self)))

(define (index size)
  (make <index> #:sym (gensym) #:size size))

(define-class <lambda> (<tensor>)
  (index #:init-keyword #:index #:getter index)
  (term #:init-keyword #:term #:getter term))

(define-method (write (self <lambda>) port)
  (format port "~a" (tensor->list self)))

(define-method (lamb index term)
  (make <lambda> #:index index #:term term))

(define-class <lookup> (<tensor>)
  (index #:init-keyword #:index #:getter index)
  (term #:init-keyword #:term #:getter term)
  (stride #:init-keyword #:stride #:getter stride))

(define-method (write (self <lookup>) port)
  (format port "(lookup ~a ~a ~a)" (index self) (term self) (stride self)))

(define-class <func> (<tensor>)
  (args #:init-keyword #:args #:getter args))

(define-method (write (self <func>) port)
  (format port "~a" (tensor->list self)))

(define-class <reduction> (<tensor>)
  (index #:init-keyword #:index #:getter index)
  (term #:init-keyword #:term #:getter term))

(define-method (write (self <reduction>) port)
  (format port "~a" (tensor->list self)))

(define-method (reduction i expression)
  (make <reduction> #:index i #:term expression))

(define-method (reduction i (expression <lambda>))
  (lamb (index expression) (reduction i (term expression))))

(define-syntax sum
  (lambda (x)
    (syntax-case x ()
      ((k (i s) expression) #'(let [(i (index s))] (reduction i expression)))
      ((k i expression) #'(let [(i (index 0))] (reduction i expression))))))

(define-method (size (self <reduction>))
  (size (term self)))

(define-method (lookup index term stride)
  (make <lookup> #:index index #:term term #:stride stride))

(define-method (lookup idx (t <lambda>) stride)
  (lamb (index t) (lookup idx (term t) stride)))

(define-method (fetch (p <foreign>))
  (bytevector-u8-ref (pointer->bytevector p 1) 0))

(define-method (+ (a <foreign>) (b <integer>))
  (make-pointer (+ (pointer-address a) b)))

(define-method (get (x <foreign>)) (fetch x))

(define-method (get (x <reduction>))
  (apply + (map (lambda (i) (get (subst (term x) (index x) i))) (iota (size (index x))))))

(define-method (get (x <lambda>)) x)

(define-method (get (x <lookup>)) x)

(define-method (get (x <integer>)) x)

(define-method (get (x <lambda>) i . args)
  (let [(args (cons i args))]
    (apply get (subst (term x) (index x) (last args)) (all-but-last args))))

(define-method (get (x <func>) . a)
  (apply + (map (lambda (arg) (apply get arg a)) (args x))))

(define-syntax tensor
  (lambda (x)
    (syntax-case x ()
      ((k (i s) expression) #'(let [(i (index s))] (lamb i expression)))
      ((k i expression) #'(let [(i (index 0))] (lamb i expression))))))

(define-method (subst (x <lambda>) (idx <index>) i)
  (lamb (index x) (subst (term x) idx i)))

(define-method (subst (x <foreign>) (idx <index>) i)
  x)

(define-method (subst (x <index>) (idx <index>) i)
  (if (eq? x idx) i x))

(define-method (subst (x <integer>) (idx <index>) i)
  x)

(define-method (subst (x <lookup>) (idx <index>) (i <integer>))
  (if (eq? idx (index x))
    (rebase (term x) (* i (stride x)))
    (lookup (index x) (subst (term x) idx i) (stride x))))

(define-method (subst (x <lookup>) (idx <index>) (i <index>))
  (if (eq? idx (index x))
    (begin
      (slot-set! i 'size (size idx))
      (lookup i (term x) (stride x)))
    (lookup (index x) (subst (term x) idx i) (stride x))))

(define-method (subst (x <reduction>) (idx <index>) i)
  (reduction (index x) (subst (term x) idx i)))

(define-method (subst (x <func>) (idx <index>) i)
  (make <func> #:args (map (lambda (arg) (subst arg idx i)) (args x))))

(define-method (rebase (x <lookup>) offset)
  (lookup (index x) (rebase (term x) offset) (stride x)))

(define-method (rebase (x <foreign>) offset)
  (+ x offset))

(define-method (+ (a <tensor>) (b <tensor>))
  (make <func> #:args (list a b)))

(define-method (+ (a <lambda>) (b <lambda>))
  (tensor i (+ (get a i) (get b i))))

(define (arr->tensor a)
  (if (zero? (dimensions a))
    (memory a)
    (let [(i (index (last (shape a))))]
      (lamb i (lookup i (arr->tensor (project a)) (last (strides a)))))))

(define-method (size (x <lambda>)) (size (index x)))

(define-method (tensor->list t) (get t))

(define-method (tensor->list (t <lambda>))
  (map (lambda (i) (tensor->list (get t i))) (iota (size t))))

(define m (arr 1 2 3))
(define t (arr->tensor m))
(tensor i (get t i))
(tensor i (+ (get t i) (get t i)))
(+ t t)
(tensor i (tensor j (+ (get t i) (get t j))))
(tensor i (+ (get t i) i))
(tensor i (+ i (get t i)))
(sum i (get t i))
(sum i (+ (get t i) (get t i)))
(+ (sum i (get t i)) (sum j (get t j)))

(sum (i 5) i)

(tensor (i 5) i)
(tensor (j 3) (tensor (i 5) i))
(tensor (j 3) (tensor (i 5) j))
(tensor (j 3) (tensor (i 5) (+ i j)))

(define n (arr (1 2 3) (4 5 6)))
(define u (arr->tensor n))
(+ u u)
(tensor i (tensor j (get u i j)))
(tensor i (tensor j (get (get u j) i)))

(tensor i (sum j (get u i j)))
(sum i (tensor j (get u i j)))
