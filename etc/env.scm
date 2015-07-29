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


;(template-class)
; typed fragments?

(define-class <meta<fragment<>>> (<meta<element>>))
(define-class <fragment<>> (<element>)
              (type #:init-keyword #:type #:getter type)
              (value #:init-keyword #:value #:getter get-value)
              (code #:init-keyword #:code #:getter get-code)
              #:metaclass <meta<fragment<>>>)

(define (fragment type)
  (let* [(name       (format #f "<fragment~a>" (class-name type)))
         (metaname   (format #f "<meta~a>" name))
         (metaclass  (def-once metaname (make <class>
                                              #:dsupers (list <meta<fragment<>>>)
                                              #:name metaname)))
         (retval     (def-once name (make metaclass
                                          #:dsupers (list <fragment<>>)
                                          #:name name)))]
    retval))

(define-class <fragment> ()
  (type #:init-keyword #:type #:getter type)
  (value #:init-keyword #:value #:getter get-value)
  (code #:init-keyword #:code #:getter get-code))

(define-method (parameter s)
  (make <fragment> #:type (class-of s)
                   #:value s
                   #:code (lambda (result) '())))
(define-method (parameter (var <var>))
  (make <fragment> #:type (typecode var)
                   #:value var
                   #:code (lambda (result) '())))
; use <element>, <int>, ...?

(define (temporary frag)
  (or (get-value frag) (make <var> #:type (type frag))))

(define-method (typecast (target <meta<element>>) (frag <fragment>))
  (let [(tmp (temporary frag))
        (mov (if (>= (size-of (type frag)) (size-of target))
                 MOV
                 (if (signed? (type frag))
                     MOVSX
                     (if (>= (size-of (type frag)) 4) MOV MOVZX))))]
    (make <fragment> #:type target
                     #:value #f
                     #:code (lambda (result)
                                    (append ((get-code frag) tmp) (list (mov result tmp)))))))

((get-code (typecast <int> (parameter u))) a)

;(define-syntax-rule (element-wise (type p start n step) body ...)
;  (env [(delta <long>)
;        (stop  <long>)
;        (incr  <long>)]
;    (MOV delta n)
;    (IMUL delta step)
;    (LEA stop (ptr type start delta))
;    (IMUL incr step (size-of type))
;    (for [(p <long>) (MOV p start) (CMP p stop) (ADD p incr)] body ...)))

(define-method (dereference (self <var>)) self)
(define-method (dereference (self <pointer<>>)) (ptr (typecode self) (get-value self)))

(define-method (+ (a <fragment>) (b <fragment>))
   (let* [(target  (coerce (type a) (type b)))
          (tmp     (make <var> #:type target))]
   (make <fragment> #:type target
                    #:value #f
                    #:code (lambda (result)
                                   (append ((get-code (typecast target a)) result)
                                           ((get-code (typecast target b)) tmp)
                                   (list (ADD result tmp)))))))

((get-code (+ (parameter u) (parameter b))) a)

;(define-method (dereference (self <var>)) self)
(define-method (dereference (self <pointer<>>)) (ptr (typecode self) (get-value self)))

(define a (make <var> #:type <int> #:symbol 'a))
(define s (param (sequence <int>) (list x y z)))
(define r (param (sequence <int>) (list o p q)))

(parameter s)

(define-method (++ (a <fragment>) (b <fragment>))
  (let* [(tmp  (make <var> #:type (typecode (type a))))
         (body (+ (parameter tmp) b))
         (r    (make <var> #:type (type body)))]
    (make <fragment> #:type (type a)
                     #:value #f
                     #:code (lambda (result)
                                    (append (list (MOV tmp (get-value a)))
                                            ((get-code body) r)
                                            (list (MOV result r)))))))

(define-method (+++ (a <fragment>) (b <fragment>))
  (let* [(target (coerce (type a) (type b)))]
    (make <fragment> #:type target
                     #:value #f
                     #:code (lambda (result)
                                    ((get-code (++ (parameter (project (get-value a))) b))
                                     (project result))))))

((get-code (+++ (parameter s) (parameter a))) r)



(types (sequence <int>))

(define s (param (sequence <int>) (list x y z)))
(get-value s)
(shape s)
(strides s)

(a+ (parameter s) (parameter a))



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
