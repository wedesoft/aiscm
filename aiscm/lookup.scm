(define-module (aiscm lookup)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm var)
  #:export (<lookup>
            make-lookup
            get-offset
            get-stride
            get-length))
(define-class <lookup> (<element>)
  (offset #:init-keyword #:offset #:getter get-offset)
  (stride #:init-keyword #:stride #:getter get-stride))
(define (make-lookup value offset stride)
  (make <lookup> #:value value #:offset offset #:stride stride))
(define-method (equal? (a <lookup>) (b <lookup>))
  (and
    (next-method)
    (equal? (get-offset a) (get-offset b))
    (equal? (get-stride a) (get-stride b))))
(define-method (lookup (self <pointer<>>) (value <var>) (stride <integer>))
  (make-lookup self value stride))
;(define-method (skip (self <pointer<>>) (offset <integer>))
;  (make-lookup (lookup (get-value self) offset (get-stride self)) (get-offset self) (get-stride self)))
(define-method (subst (self <lookup>) alist)
  (lookup (get-value self) (subst (get-offset self) alist) (get-stride self)))
(define-method (typecode (self <lookup>))
  (typecode (get-value self)))
