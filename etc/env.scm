(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm sequence) (aiscm pointer) (aiscm element) (aiscm int) (aiscm op) (aiscm util) (srfi srfi-1) (guile-tap))

(define-class <lookup> ()
  (term   #:init-keyword #:term   #:getter term)
  (index  #:init-keyword #:index  #:getter index)
  (stride #:init-keyword #:stride #:getter stride)
  (last-shape #:init-keyword #:last-shape #:getter last-shape))
(define (lookup term index stride last-shape)
  (make <lookup> #:term term #:index index #:stride stride #:last-shape last-shape))

(define-class <tensor> ()
  (index #:init-keyword #:index #:getter index)
  (term  #:init-keyword #:term  #:getter term))
(define (tensor index term)
  (make <tensor> #:index index #:term term))

(define-method (express (self <element>)) self); could be "skeleton" later
(define-method (express (self <sequence<>>))
  (let [(i <var>)]
    (tensor i (lookup (project self) i (last (strides self)) (last (shape self))))))

(define-class <elementwise> ()
  (setup     #:init-keyword #:setup     #:getter get-setup)
  (increment #:init-keyword #:increment #:getter get-increment)
  (body      #:init-keyword #:body      #:getter get-body))

(define-method (element-wise (self <tensor>))
  (element-wise (term self) (index self)))
(define-method (typecode (self <lookup>)) (typecode (term self)))
(define-method (ppp (self <lookup>)) (ppp (term self)))
(define-method (ppp (self <pointer<>>)) (get self))
(define-method (type (self <pointer<>>)) (typecode self))
(define-method (type (self <lookup>)) (type (term self)))
(define-method (type (self <tensor>)) (sequence (type (term self))))
(define-method (last-shape (self <tensor>)) (last-shape (term self)))
(define-method (last-shape (self <sequence<>>)) (last (shape self)))
(define-method (subst (self <pointer<>>) (p <var>))
  (make (class-of self) #:value p))
(define-method (element-wise (s <sequence<>>))
  (let [(incr (var <long>))
        (p    (var <long>))]
    (make <elementwise>
          #:setup (list (IMUL incr (last (strides s)) (size-of (typecode s)))
                        (MOV p (slot-ref s 'value)))
          #:increment (list (ADD p incr))
          #:body (project (rebase p s)))))
(define-method (element-wise (self <lookup>))
  (let [(incr (var <long>))
        (p    (var <long>))]
    (make <elementwise>
      #:setup (list (IMUL incr (stride self) (size-of (typecode self)))
                    (MOV p (ppp self)))
      #:increment (list (ADD p incr))
      #:body (subst (term self) p))))
(define-method (element-wise (self <lookup>) (i <var>))
  (element-wise self))
(define-method (element-wise (self <tensor>))
  (element-wise (term self)))

; tensor/lookup/element-wise <-> code fragments
(define-method (store (p <pointer<>>) (a <pointer<>>))
  (let [(result (var (typecode a)))]
    (list (MOV result (ptr (typecode a) (get a))) (MOV (ptr (typecode p) (get p)) result))))
(define-method (store (result <sequence<>>) expr)
  (let [(destination (element-wise result))
        (source      (element-wise expr))]
    (list (get-setup destination)
          (get-setup source)
          (repeat (last-shape result)
                  (append (store (get-body destination) (get-body source)))
                          (get-increment destination)
                          (get-increment source)))))

(define ctx (make <context>))

(define r (skeleton (sequence <int>)))
(define s (skeleton (sequence <int>)))
(define c (skeleton <int>))
(define i (var <int>))

(define-method (returnable self) #f)
(define-method (returnable (self <meta<int<>>>)) self)
(define (assemble retval vars frag)
  (virtual-variables (if (returnable (class-of retval)) (list (get retval)) '())
                     (concatenate (map content (if (returnable (class-of retval)) vars (cons retval vars))))
                     (append (store retval frag) (list (RET)))))
(define (jit context classes proc)
  (let* [(vars        (map skeleton classes))
         (frag        (apply proc (map express vars)))
         (result-type (type frag))
         (return-type (returnable result-type))
         (target      (if return-type result-type (pointer result-type)))
         (retval      (skeleton target))
         (args        (if return-type vars (cons retval vars)))
         (code        (asm context
                           (or return-type <null>)
                           (map typecode (concatenate (map content args)))
                           (assemble retval vars frag)))
         (fun         (lambda header (apply code (concatenate (map content header)))))]
    (if return-type
        (lambda args
          (let [(result (apply fun args))]
            (get (build result-type result))))
        (lambda args
          (let [(result (make target #:shape (argmax length (map shape args))))]
            (apply fun (cons result args))
            (get (build result-type result)))))))

(element-wise (express s))

(store r (express s))

(assemble r (list s) (express s))

((jit ctx (list (sequence <int>)) (lambda (s) s)) (seq <int> 2 3 5))

(ok (eq? c (express c))
    "Scalar expresses itself")
(ok (is-a? (express s) <tensor>)
    "Sequence is expressed as tensor")
