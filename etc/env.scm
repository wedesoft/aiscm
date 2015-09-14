(use-modules (oop goops) (srfi srfi-1) (srfi srfi-26) (ice-9 optargs) (ice-9 curried-definitions) (aiscm util) (aiscm element) (aiscm pointer) (aiscm mem) (aiscm sequence) (aiscm asm) (aiscm jit) (aiscm op) (aiscm int) (aiscm float) (aiscm rgb))

(define ctx (make <context>))

;((jit ctx (list (sequence <intrgb>)) identity) (seq <intrgb> (rgb 1 2 3)))

(pointer <rgb<>>)

(define retval (skel (sequence <intrgb>)))
(define arg (skel (sequence <intrgb>)))
(define fragment (parameter arg))
(define code (store retval fragment))


(define-method (parameter (p <pointer<rgb<>>>))
  (make (fragment (typecode (class-of p)))
        #:args (list p)
        #:name parameter
        #:code (lambda (result) (list (NOP) (NOP) (MOV (red result) (ptr (type (typecode (class-of p))) (get-value p)))))))

(store retval fragment)

(virtual-variables '() (concatenate (map decompose (list retval arg))) (append code (list (RET))))

(skel <intrgb>)

(project (skel (sequence <intrgb>)))

;(define-method (store ))

(define c (list (list (rgb 2 3 5) (rgb 7 11 13)) (list (rgb 3 5 7) (rgb 5 7 11))))
(to-list (to-array (to-image (to-array <intrgb> c))))

(define-method (to-type (target <meta<rgb<>>>) (frag <fragment<rgb<>>>))
  (make (fragment (to-type target))
        #:args
        #:name to-type
        #:code (lambda (result) result)))

(define-method (to-type (target <meta<element>>) (frag <fragment<element>>))
  (let* [(source (typecode (type (class-of frag))))
         (tmp    (make <var> #:type source))
         (mov    (if (>= (size-of source) (size-of target))
                     MOV
                     (if (signed? source)
                         MOVSX
                         (if (>= (size-of source) 4) MOV MOVZX))))]
    (make (fragment (to-type target (type (class-of frag))))
          #:args (list target frag)
          #:name to-type
          #:code (lambda (result)
                         (append ((code frag) tmp) (list (mov result tmp)))))))


; macros: (jit-method [(x <int>) (y <float>)] (+ x y))

;(define-syntax let-vars
;  (syntax-rules ()
;    ((let-vars [(name type) vars ...] body ...)
;     (let [(name (make <var> #:type type #:symbol (quote name)))]
;       (let-vars [vars ...] body ...)))
;    ((let-vars [] body ...)
;     (begin body ...))))


(define-syntax test
  (syntax-rules ()
    ((test ((a b) args ...))
     (cons b (test (args ...))))
    ((test  ())
     '())))

(define-syntax test
  (syntax-rules ()
    ((test ((a b) args ...))
     (cons b (test (args ...))))
    ((test  ())
     '())))

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
