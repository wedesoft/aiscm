(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (ice-9 optargs) (ice-9 curried-definitions) (aiscm util) (aiscm element) (aiscm pointer) (aiscm mem) (aiscm sequence) (aiscm asm) (aiscm jit) (aiscm op) (aiscm int) (aiscm float))

(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))
(define u (make <var> #:type <byte> #:symbol 'u))
(define v (make <var> #:type <byte> #:symbol 'v))
(define x (make <var> #:type <long> #:symbol 'x))
(define y (make <var> #:type <long> #:symbol 'y))
(define z (make <var> #:type <long> #:symbol 'z))
(define o (make <var> #:type <long> #:symbol 'o))
(define p (make <var> #:type <long> #:symbol 'p))
(define q (make <var> #:type <long> #:symbol 'q))

(define *p (make (pointer <int>) #:value p))
(define *q (make (pointer <int>) #:value q))

(define-method (assemble (retval <pointer<>>) vars fragment)
  (let* [(target (typecode (class-of retval)))
         (tmp    (make <var> #:type target))]
    (virtual-variables '()
                       (concatenate (map decompose (cons retval vars)))
                       (append ((code fragment) tmp)
                               (list (MOV (ptr target (get-value retval)) tmp) (RET))))))




(define-class* <fragment<element>> <object> <meta<fragment<element>>> <class>
              (value #:init-keyword #:value #:getter get-value)
              (code #:init-keyword #:code #:getter get-code))

(define (fragment t)
  (template-class (fragment t) (fragment (super t))
    (lambda (class metaclass)
      (define-method (type (self metaclass)) t))))

(define-method (parameter s)
  (make (fragment (class-of s))
        #:value s
        #:code (lambda (result) '())))
(define-method (parameter (var <var>))
  (make (fragment (typecode var))
        #:value var
        #:code (lambda (result) '())))
; TODO: use <element>, <int>, ...?

(define (temporary frag)
  (or (get-value frag) (make <var> #:type (type frag))))

(define-method (typecast (target <meta<element>>) (frag <fragment<element>>))
  (let [(tmp (temporary frag))
        (mov (if (>= (size-of (type (class-of frag))) (size-of target))
                 MOV
                 (if (signed? (type (class-of frag)))
                     MOVSX
                     (if (>= (size-of (type (class-of frag))) 4) MOV MOVZX))))]
    (make (fragment target)
          #:value #f
          #:code (lambda (result)
                         (append ((get-code frag) tmp) (list (mov result tmp)))))))

((get-code (typecast <int> (parameter u))) a)

(define-method (+ (a <fragment<element>>) (b <fragment<element>>))
   (let* [(target  (coerce (type (class-of a)) (type (class-of b))))
          (tmp     (make <var> #:type target))]
   (make (fragment target)
         #:value #f
         #:code (lambda (result)
                        (append ((get-code (typecast target a)) result)
                                ((get-code (typecast target b)) tmp)
                        (list (ADD result tmp)))))))

((get-code (+ (parameter u) (parameter b))) a)

; (define-method (dereference (self <var>)) self)
; (define-method (dereference (self <pointer<>>)) (ptr (typecode self) (get-value self)))

(types (sequence <int>))

(define a (make <var> #:type <int> #:symbol 'a))
(define s (param (sequence <int>) (list x y z)))
(define r (param (sequence <int>) (list o p q)))

(get-value s)
(shape s)
(strides s)

(parameter s)
(parameter (project s))

(define-method (+ (a <fragment<pointer<>>>) (b <fragment<element>>))
  (let* [(tmp  (make <var> #:type (typecode (type (class-of a)))))
         (body (+ (parameter tmp) b))
         (r    (make <var> #:type (type (class-of body))))]
    (make (class-of a)
          #:value #f
          #:code (lambda (result)
                         (append (list (MOV tmp (get-value a)))
                                 ((get-code body) r)
                                 (list (MOV result r)))))))

((get-code (+ (parameter (project s)) (parameter a))) (project r))

(define-syntax-rule (element-wise (type p start n step) body ...)
  (env [(delta <long>)
        (stop  <long>)
        (incr  <long>)]
    (MOV delta n)
    (IMUL delta step)
    (LEA stop (ptr type start delta))
    (IMUL incr step (size-of type))
    (for [(p <long>) (MOV p start) (CMP p stop) (ADD p incr)] body ...)))

(define-method (+ (a <fragment<sequence<>>>) (b <fragment<element>>))
  (let* [(target (coerce (type (class-of a)) (type (class-of b))))]
    (make (fragment target)
          #:value #f
          #:code (lambda (result)
                         (env [(*a <long>)
                               (a+ <long>)]
                              (MOV *a (get-value (get-value a)))
                              (MOV a+ (last (strides (get-value a))))
                              (IMUL a+ a+ (size-of (typecode (get-value a))))
                              (element-wise ((typecode result) *r
                                            (get-value result) (last (shape result))
                                            (last (strides result)))
                                      ((get-code (+ (parameter (project (rebase *a (get-value a)))) b))
                                       (project (rebase *r result)))
                                      (ADD *a a+)))))))

((get-code (+ (parameter s) (parameter a))) r)





; --------------------------------------------------------------------------------
; (tensor [i j] (* (s i) (s j)))
; (code (* (s i) (s j)) target) = (append (code (s i) a) (code (s j) b) (mov target a) (imul target b))
; (type (* (s i) (s j))) = (coerce (type (s i)) (type (s j)))

(define-syntax tensor-list
  (syntax-rules ()
    ((_ arg args ...) (cons (tensor arg) (tensor-list args ...)))
    ((_) '())))

(define-syntax tensor
  (syntax-rules (*)
    ((_ (* args ...)) (cons '* (tensor-list args ...)))
    ((_ arg)          arg)))

(define s (seq 1 2 3 4))
(define-syntax-rule (tensor sym args ...)
  (if (is-a? sym <element>) (get sym args ...) (sym args ...)))
(tensor + s 1)
(tensor s 1)
(tensor (seq 1 2 3 4) 1)
(tensor + (s 1) 1)

; arrays
(make-array 0 2 3)
(make-typed-array 'u8 0 2 3)
#vu8(1 2 3)
#2((1 2 3) (4 5 6))

#2u32((1 2 3) (4 5 6))
(define m #2s8((1 -2 3) (4 5 6)))
(array-ref m 1 0)
(array-shape m)
(array-dimensions m)
(array-rank m)
(array->list m)

; monkey patching
(class-slots <x>)
(define m (car (generic-function-methods test)))
((method-procedure m) x)
(slot-ref test 'methods)
;(sort-applicable-methods test (compute-applicable-methods test (list x)) (list x))
(equal? (map class-of (list x)) (method-specializers m))
(define x (make <x>))
(test x)
(define-method (test (x <x>)) 'test2)
(test x)
